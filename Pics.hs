{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Pics ( Config
            , PicDir(..)
            , Image(..)
            , FolderClass(..)
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

--import Import
import Prelude
import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import qualified Data.Text as Text

import Control.Monad
import qualified Data.Map as Map
import Data.List
import Data.Maybe
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
    { cfgDirs            :: [FilePath]
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
         v .: "dirs" <*>
         v .: "blacklisteddirs" <*>
         v .: "rawexts" <*>
         v .: "jpegexts" <*>
         v .: "sidecarexts" <*>
         v .: "otherexts" <*>
         v .: "dirregex"

  parseJSON _ = mzero

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
    { imgName        :: String
    , imgParent      :: String
    , imgRawPath     :: Maybe FilePath
    , imgSidecarPath :: Maybe FilePath
    , imgJpegPath    :: Maybe FilePath
    }

data PicDir = PicDir
  { pdName :: String
  , pdPaths :: [FilePath]
  , pdImages :: Map.Map String Image
  }

type Repository = Map.Map String PicDir

data FolderClass = FolderEmpty
                 | FolderRaw
                 | FolderStandalone
                 | FolderUnprocessed
                 | FolderProcessed
                 | FolderMixed
                   deriving (Show, Read, Eq, Ord)

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
numRawPics =
  length . computeRawPics

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

numUnprocessedPics :: PicDir -> Int
numUnprocessedPics = length . computeUnprocessedPics

computeProcessedPics :: PicDir -> [Image]
computeProcessedPics =
  filter isProcessed . Map.elems . pdImages

numProcessedPics :: PicDir -> Int
numProcessedPics = length . computeProcessedPics

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
numStandalonePics =
  length . computeStandalonePics

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
      conditions = (has_pics, all_unproc, has_unproc, has_standalone)
  in case conditions of
       (False, _   , _    , _    ) -> FolderEmpty
       (True, True , True , False) -> FolderRaw
       (True, False, True , False) -> FolderUnprocessed
       (True, False, False, True ) -> FolderStandalone
       (True, False, True , True ) -> FolderMixed
       (True, False, False, False) -> FolderProcessed
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

loadDir :: Config -> String -> FilePath -> IO PicDir
loadDir config name path = do
  contents <- recursiveScanDir config path
  let rawe = rawExtsRev config
      side = sidecarExtsRev config
      jpeg = jpegExtsRev config
      images = catMaybes $
               map (\f -> let (_, fname) = splitFileName f
                              base = dropExtensions fname
                              f' = reverse f
                              nfp = if hasExts f' rawe
                                      then Just f
                                      else Nothing
                              sdc = if hasExts f' side
                                      then Just f
                                      else Nothing
                              jpe = if hasExts f' jpeg
                                      then Just f
                                      else Nothing
                          in case (nfp, jpe) of
                               (Nothing, Nothing) -> Nothing
                               _ -> Just (base, Image base name nfp sdc jpe)
                   ) contents
  return $ PicDir name [path] (Map.fromListWith mergePictures images)

scanAll :: Config -> IO (Map.Map String PicDir)
scanAll config = do
  foldM (\r d -> scanDir config r d) Map.empty $ cfgDirs config

computeUnprocessedDirs :: Repository -> [PicDir]
computeUnprocessedDirs =
  filter hasUnprocessedPics . Map.elems

computeStandaloneDirs :: Repository -> [PicDir]
computeStandaloneDirs =
  filter hasStandalonePics . Map.elems

totalPicsOfType :: (Image -> Bool) -> Repository -> Integer
totalPicsOfType extractor =
  Map.foldr (\dir a -> Map.foldr (\i a' -> if extractor i
                                             then a' + 1
                                             else a') a (pdImages dir)
            ) 0

totalPics :: Repository -> Integer
totalPics = totalPicsOfType (const True)

totalRawPics :: Repository -> Integer
totalRawPics = totalPicsOfType (isJust . imgRawPath)

totalUnprocessedPics :: Repository -> Integer
totalUnprocessedPics = totalPicsOfType isUnprocessed

totalStandalonePics :: Repository -> Integer
totalStandalonePics = totalPicsOfType isStandalone

totalProcessedPics :: Repository -> Integer
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
