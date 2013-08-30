{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Pics ( PicDir(..)
            , Image(..)
            , scanAll
            , isProcessed
            , isUnprocessed
            , isStandalone
            , folderClass
            , computeFolderStats
            , computeUnprocessedDirs
            , computeStandaloneDirs
            , numPics
            , numRawPics
            , numUnprocessedPics
            , numStandalonePics
            , numProcessedPics
            , totalPics
            , totalUnprocessedPics
            , totalProcessedPics
            , totalRawPics
            , totalStandalonePics
            , filterDirsByClass
            ) where

import Types

--import Import
import Prelude
import Control.Applicative ((<$>), (<*>))
import Control.Concurrent.MVar
import Data.Aeson
import qualified Data.Text as T
import Data.Text (Text)
import System.IO.Unsafe

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Posix.Files
import qualified Text.Regex.PCRE as PCRE

addRevDot :: [FilePath] -> [FilePath]
addRevDot = map (reverse . ('.':))

rawExtsRev :: Config -> [FilePath]
rawExtsRev = addRevDot . cfgRawExts

jpegExtsRev :: Config -> [FilePath]
jpegExtsRev = addRevDot . cfgJpegExts

sidecarExtsRev :: Config -> [FilePath]
sidecarExtsRev = addRevDot . cfgSidecarExts

hasExts:: FilePath -> [FilePath] -> Bool
hasExts p = any (`isPrefixOf` p)

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

data Image = Image
    { imgName        :: !Text
    , imgParent      :: !Text
    , imgRawPath     :: !(Maybe Text)
    , imgSidecarPath :: !(Maybe Text)
    , imgJpegPath    :: !(Maybe Text)
    }

data PicDir = PicDir
  { pdName   :: !Text
  , pdPaths  :: !([Text])
  , pdImages :: !(Map.Map Text Image)
  }

type Repository = Map.Map Text PicDir

{-# NOINLINE repoCache #-}
repoCache :: MVar (Maybe Repository)
repoCache = unsafePerformIO $ newMVar Nothing

mergePictures :: Image -> Image -> Image
mergePictures x y =
  x { imgRawPath     = imgRawPath     x `mplus` imgRawPath     y
    , imgSidecarPath = imgSidecarPath x `mplus` imgSidecarPath y
    , imgJpegPath    = imgJpegPath    x `mplus` imgJpegPath    y
    }

mergeFolders :: PicDir -> PicDir -> PicDir
mergeFolders x y =
  x { pdPaths = pdPaths x ++ pdPaths y
    , pdImages = Map.unionWith mergePictures (pdImages x) (pdImages y)
    }

computeRawPics :: PicDir -> [Image]
computeRawPics =
  filter (isJust . imgRawPath) . Map.elems . pdImages

numRawPics :: PicDir -> Int
numRawPics = numPicsOfType (isJust . imgRawPath)

isUnprocessed :: Image -> Bool
isUnprocessed (Image { imgRawPath = Just _, imgJpegPath = Nothing }) = True
isUnprocessed _ = False

isProcessed :: Image -> Bool
isProcessed (Image { imgRawPath = Just _, imgJpegPath = Just _ }) = True
isProcessed _ = False

numPics :: PicDir -> Int
numPics = Map.size . pdImages

computeUnprocessedPics :: PicDir -> [Image]
computeUnprocessedPics =
  filter isUnprocessed . Map.elems . pdImages

numPicsOfType :: (Image -> Bool) -> PicDir -> Int
numPicsOfType criterion =
  Map.foldl go 0 . pdImages
    where go a i = if criterion i then a + 1 else a

numUnprocessedPics :: PicDir -> Int
numUnprocessedPics = numPicsOfType isUnprocessed

computeProcessedPics :: PicDir -> [Image]
computeProcessedPics =
  filter isProcessed . Map.elems . pdImages

numProcessedPics :: PicDir -> Int
numProcessedPics = numPicsOfType isProcessed

hasUnprocessedPics :: PicDir -> Bool
hasUnprocessedPics =
  not . null . computeUnprocessedPics

isStandalone :: Image -> Bool
isStandalone (Image { imgRawPath = Nothing, imgJpegPath = Just _ }) = True
isStandalone _ = False

computeStandalonePics :: PicDir -> [Image]
computeStandalonePics =
  filter isStandalone . Map.elems . pdImages

numStandalonePics :: PicDir -> Int
numStandalonePics = numPicsOfType isStandalone

hasStandalonePics :: PicDir -> Bool
hasStandalonePics =
  not . null . computeStandalonePics

folderClass :: PicDir -> FolderClass
folderClass dir =
  let npics = numPics dir
      has_pics = npics /= 0
      unproc = numUnprocessedPics dir
      has_unproc = unproc /= 0
      has_standalone = numStandalonePics dir /= 0
      all_unproc = unproc == npics
      has_raw = numRawPics dir /= 0
      conditions = (has_pics, all_unproc, has_unproc, has_standalone, has_raw)
  in case conditions of
       (False, _   , _    , _    , _    ) -> FolderEmpty
       (True, True , True , False, _    ) -> FolderRaw
       (True, False, True , False, _    ) -> FolderUnprocessed
       (True, False, False, True , False) -> FolderStandalone
       (True, False, _    , True , True ) -> FolderMixed
       (True, False, False, False, _    ) -> FolderProcessed
       _ -> error $ "Wrong computation in folderClass: " ++ show conditions

getDownContents :: Config -> FilePath -> IO [FilePath]
getDownContents config base = do
  contents <- getDirectoryContents base
  let blkdirs = blacklistedDirs config
  return $ filter (\d -> d `notElem` blkdirs) contents

isDir :: FilePath -> IO Bool
isDir = liftM isDirectory . getSymbolicLinkStatus

scanDir :: Config
        -> Repository
        -> String
        -> IO Repository
scanDir config repo base = do
  paths <- getDownContents config base
  dirs <- filterM (isDir . (base </>)) paths
  foldM (\r p -> scanSubDir config r (base </> p)) repo dirs

scanSubDir :: Config
           -> Repository
           -> String
           -> IO Repository
scanSubDir config repository path = do
  allpaths <- getDownContents config path
  dirpaths <- filterM (isDir . (path </>)) allpaths
  let allpaths' = filter (isOKDir config) dirpaths
  foldM (\r s -> do
           dir <- loadDir config s (path </> s)
           return $ Map.insertWith mergeFolders (pdName dir) dir r)
        repository allpaths'

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

-- | Strict application of the 'Just' constructor. This is useful as
-- the Maybe type is not strict in its contained value.
strictJust :: a -> Maybe a
strictJust !a = Just a

loadDir :: Config -> String -> FilePath -> IO PicDir
loadDir config name path = do
  contents <- recursiveScanDir config path
  let rawe = rawExtsRev config
      side = sidecarExtsRev config
      jpeg = jpegExtsRev config
      tname = T.pack name
      loadImage f = let base = dropExtensions f
                        tbase = T.pack base
                        f' = reverse f
                        tf = T.pack f
                        jtf = strictJust tf
                        nfp = if hasExts f' rawe
                                then jtf
                                else Nothing
                        sdc = if hasExts f' side
                                then jtf
                                else Nothing
                        jpe = if hasExts f' jpeg
                                then jtf
                                else Nothing
                    in case (nfp, jpe) of
                         (Nothing, Nothing) -> Nothing
                         _ -> Just (tbase, Image tbase tname nfp sdc jpe)
      images = foldl' (\acc f ->
                         case loadImage f of
                           Nothing -> acc
                           Just (k, img) ->
                             Map.insertWith mergePictures k img acc
                      ) Map.empty contents
  return $ PicDir tname [T.pack path] images

scanFilesystem :: Config -> IO Repository
scanFilesystem config = do
  foldM (\r d -> scanDir config r d) Map.empty $ cfgDirs config

maybeUpdateCache :: Config
                 -> Maybe Repository
                 -> IO (Maybe Repository, Repository)
maybeUpdateCache config Nothing = do
  r <- scanFilesystem config
  return (Just r, r)
maybeUpdateCache _ orig@(Just r) = return (orig, r)

scanAll :: Config -> IO Repository
scanAll config = do
  modifyMVar repoCache (maybeUpdateCache config)

computeUnprocessedDirs :: Repository -> [PicDir]
computeUnprocessedDirs =
  filter hasUnprocessedPics . Map.elems

computeStandaloneDirs :: Repository -> [PicDir]
computeStandaloneDirs =
  filter hasStandalonePics . Map.elems

totalPicsOfType :: (Image -> Bool) -> Repository -> Int
totalPicsOfType criterion =
  Map.foldl (\a dir ->
                 a + numPicsOfType criterion dir
            ) 0

totalPics :: Repository -> Int
totalPics = totalPicsOfType (const True)

totalRawPics :: Repository -> Int
totalRawPics = totalPicsOfType (isJust . imgRawPath)

totalUnprocessedPics :: Repository -> Int
totalUnprocessedPics = totalPicsOfType isUnprocessed

totalStandalonePics :: Repository -> Int
totalStandalonePics = totalPicsOfType isStandalone

totalProcessedPics :: Repository -> Int
totalProcessedPics = totalPicsOfType isProcessed

computeFolderStats :: Repository -> Map.Map FolderClass Int
computeFolderStats =
  Map.fromListWith (+) .
  map ((, 1) . folderClass) .
  Map.elems

filterDirsByClass :: [FolderClass] -> Repository -> [PicDir]
filterDirsByClass classes =
  filter ((`elem` classes) . folderClass) .
  Map.elems
