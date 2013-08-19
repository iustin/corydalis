{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Pics ( Config
            , NefDir(..)
            , JpegDir(..)
            , scanAll
            , computeUnprocessed
            , computeStandaloneJpegs
            ) where

--import Import
import Prelude
import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import qualified Data.Text as Text

import Control.Monad
import qualified Data.Map as Map
import Data.List
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Text.Regex.PCRE as PCRE

data Regex = Regex
    { reString :: String
    , reRegex  :: PCRE.Regex
    }

instance Show Regex where
  show (Regex str _) = show str

instance FromJSON Regex where
  parseJSON (String txt) =
    let str = Text.unpack txt
    in case PCRE.makeRegexM str of
         Nothing -> mzero
         Just r -> return $ Regex str r
  parseJSON _ = mzero

data Config = Config
    { cfgRawBase         :: FilePath
    , cfgJpegBase        :: FilePath
    , cfgBlacklistedDirs :: [FilePath]
    , cfgRawExts         :: [FilePath]
    , cfgJpegExts        :: [FilePath]
    , cfgSidecarExts     :: [FilePath]
    , cfgOtherImgExts    :: [FilePath]
    , cfgDirRegex        :: Regex
    } deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
         v .: "rawbase" <*>
         v .: "jpegbase" <*>
         v .: "blacklisteddirs" <*>
         v .: "rawexts" <*>
         v .: "jpegexts" <*>
         v .: "sidecarexts" <*>
         v .: "otherexts" <*>
         v .: "dirregex"

  parseJSON _ = mzero

rawExtsRev :: Config -> [FilePath]
rawExtsRev = map reverse . cfgRawExts

jpegExtsRev :: Config -> [FilePath]
jpegExtsRev = map reverse . cfgJpegExts

sidecarExtsRev :: Config -> [FilePath]
sidecarExtsRev = map reverse . cfgSidecarExts

fileExts :: Config -> [String]
fileExts config =
  concat [ cfgRawExts config
         , cfgSidecarExts config
         , cfgJpegExts config
         , cfgOtherImgExts config
         ]

fileDotExts :: Config -> [String]
fileDotExts = map ('.':) . fileExts

blacklistedDirs :: Config -> [String]
blacklistedDirs config = [".", ".."] ++ cfgBlacklistedDirs config

isOKDir :: Config -> String -> Bool
isOKDir cfg = PCRE.match (reRegex $ cfgDirRegex cfg)

data NefDir = NefDir { nfName :: String
                     , nfPath :: String
                     , nfWritten :: Bool
                     , nfNefCount :: Int
                     , nfXmpCount :: Int
                     }

data JpegDir = JpegDir { jpName :: String
                       , jpPath :: String
                       , jpJpegCount :: Int
                       }

data PicDir = PicNef NefDir
            | PicJpeg JpegDir
            | PicBoth NefDir JpegDir

type Repository = Map.Map String PicDir

class HasName a where
  nameOf :: a -> String

instance HasName NefDir where
  nameOf = nfName

instance HasName JpegDir where
  nameOf = jpName

instance HasName PicDir where
  nameOf (PicNef v) = nameOf v
  nameOf (PicJpeg v) = nameOf v
  nameOf (PicBoth v _) = nameOf v


getDownContents :: Config -> FilePath -> IO [FilePath]
getDownContents config base = do
  contents <- getDirectoryContents base
  let blkdirs = blacklistedDirs config
  return $ filter (\d -> d `notElem` blkdirs) contents

isDir :: FilePath -> IO Bool
isDir = liftM isDirectory . getSymbolicLinkStatus

scanDir :: (HasName a) =>
           Config
        -> String
        -> (String -> String -> IO a)
        -> IO (Map.Map String a)
scanDir config base builder = do
  paths <- getDownContents config base
  dirs <- filterM (isDir . (base </>)) paths
  ndirs <- mapM (\p -> scanSubDir config (base </> p) builder) dirs
  let ndirs' = concat ndirs
  return $ foldl (\a v -> Map.insert (nameOf v) v a) Map.empty ndirs'

scanSubDir :: (HasName a) =>
              Config -> String -> (String -> String -> IO a) -> IO [a]
scanSubDir config path builder = do
  allpaths <- getDownContents config path
  dirpaths <- filterM (isDir . (path </>)) allpaths
  let allpaths' = filter (isOKDir config) dirpaths
  mapM (\s -> builder s (path </> s)) allpaths'

mergeDirs :: Map.Map String NefDir -> Map.Map String JpegDir ->
             Map.Map String PicDir
mergeDirs nefs jpegs =
  let withnefs = foldl (\a n ->
                          let name = nameOf n in
                          case Map.lookup name jpegs of
                            Nothing -> Map.insert name (PicNef n) a
                            Just j -> Map.insert name (PicBoth n j) a
                       ) Map.empty $ Map.elems nefs
      withjpegs = foldl (\a j ->
                           let name = nameOf j in
                           case Map.lookup name a of
                             Nothing -> Map.insert name (PicJpeg j) a
                             Just _ -> a) withnefs $ Map.elems jpegs
  in withjpegs

recursiveScanDir :: Config -> FilePath -> IO [FilePath]
recursiveScanDir config base = do
  contents <- getDownContents config base
  let allexts = fileDotExts config
      potentialdirs =
        filter (\s -> all (\e -> not (e `isSuffixOf` s)) allexts) contents
  dirs <- filterM (isDir . (base </>)) potentialdirs
  subdirs <- mapM (\s -> recursiveScanDir config (base </> s)) dirs
  return $ contents ++ concat subdirs

isInteresting :: [FilePath] -> FilePath -> Bool
isInteresting rev_exts file = any (`isPrefixOf` file) rev_exts

loadNefDir :: Config -> String -> FilePath -> IO NefDir
loadNefDir config name path = do
  contents <- recursiveScanDir config path
  let rcontent = map reverse contents
  let nefs = length $
             filter (isInteresting (rawExtsRev config)) rcontent
      xmps = length $
             filter (isInteresting (sidecarExtsRev config)) rcontent
  return $ NefDir name path False nefs xmps

loadJpegDir :: Config -> String -> FilePath -> IO JpegDir
loadJpegDir config name path = do
  contents <- recursiveScanDir config path
  let rcontent = map reverse contents
      jpegs = length $ filter (isInteresting (jpegExtsRev config)) rcontent
  return $ JpegDir name path jpegs

numUnprocessed :: NefDir -> Int
numUnprocessed n =
  if delta > 0
    then delta
    else 0
  where delta = nfNefCount n - nfXmpCount n

showUnprocessed :: NefDir -> String
showUnprocessed n =
  if u > 0 then "(" ++ show u ++ " unprocessed )" else ""
    where u = numUnprocessed n

scanAll :: Config -> IO (Map.Map String PicDir)
scanAll config = do
  nefs <- scanDir config (cfgRawBase config) (loadNefDir config)
  jpegs <- scanDir config (cfgJpegBase config) (loadJpegDir config)
  let pics = mergeDirs nefs jpegs
  return pics

computeUnprocessed :: Repository -> [NefDir]
computeUnprocessed =
  reverse .
  foldl (\a e -> case e of
                   -- it could be (rare) that a given directory only
                   -- has movies, so nothing to process
                   PicNef n | nfNefCount n > 0 -> n:a
                   _ -> a) [] .
  Map.elems


computeStandaloneJpegs :: Repository -> [JpegDir]
computeStandaloneJpegs =
  reverse .
  foldl (\a e -> case e of
                   -- it could be (rare) that a given directory only
                   -- has movies, so nothing to process
                   PicJpeg j -> j:a
                   _ -> a) [] .
  Map.elems
