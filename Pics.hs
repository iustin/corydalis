{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}
module Pics ( PicDir(..)
            , Image(..)
            , File(..)
            , scanAll
            , forceScanAll
            , isProcessed
            , isUnprocessed
            , isOutdated
            , isStandalone
            , folderClass
            , computeFolderStats
            , computeStandaloneDirs
            , numPics
            , numRawPics
            , numUnprocessedPics
            , numOutdatedPics
            , numStandalonePics
            , numOrphanedPics
            , numProcessedPics
            , filterDirsByClass
            , computeRepoStats
            , computeTimeLine
            , Stats(..)
            ) where

import Types

--import Import
import Prelude
import Control.Concurrent.MVar
import qualified Data.Text as T
import Data.Text (Text)
import System.IO.Unsafe

import Control.Monad
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
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

blacklistedDirs :: Config -> [String]
blacklistedDirs config = [".", ".."] ++ cfgBlacklistedDirs config

isOKDir :: Config -> String -> Bool
isOKDir cfg = PCRE.match (reRegex $ cfgDirRegex cfg)

dropCopySuffix :: Config -> String -> String
dropCopySuffix cfg name =
  case PCRE.match (reRegex $ cfgCopyRegex cfg) name of
    [(_:base:_)] -> base
    _ -> name

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                [(i, [])] -> Just i
                _   -> Nothing

expandRangeFile :: Config -> String -> [String]
expandRangeFile cfg name =
  case PCRE.match (reRegex $ cfgRangeRegex cfg) name of
    [[_, base, begin, end]] -> let ib = maybeRead begin::Maybe Int
                                   ie = maybeRead end
                                   expand s = if length s >= length begin
                                                then s
                                                else expand ('0':s)
                               in case (ib, ie) of
                                    (Just b, Just e) ->
                                      [base ++ expand (show i) | i <- [b..e]]
                                    _ -> []
    _ -> []

data File = File
  { fileName  :: !Text
  , fileMTime :: !POSIXTime
  } deriving (Show)

data Image = Image
    { imgName        :: !Text
    , imgParent      :: !Text
    , imgRawPath     :: !(Maybe File)
    , imgSidecarPath :: !(Maybe File)
    , imgJpegPath    :: ![File]
    , imgRange       :: !(Maybe (Text, Text))
    , imgStatus      :: !ImageStatus
    } deriving (Show)

-- | Computes the status of an image given the files that back it
-- (raw, jpeg, sidecar).
mkImageStatus :: Config
              -> Maybe File  -- ^ Raw file
              -> [File]      -- ^ Jpeg file(s)
              -> Maybe File  -- ^ Sidecar file
              -> ImageStatus
mkImageStatus _ Nothing  []    Nothing   =
  error "imageStatus - neither raw nor standalone nor orphaned"
mkImageStatus _ Nothing  []    (Just _)  = ImageOrphaned
mkImageStatus _ Nothing  (_:_) (Just _)  = ImageStandalone
  --error "imageStatus - orphaned + jpeg?"
mkImageStatus _ (Just _) []    _         = ImageRaw
mkImageStatus _ Nothing  (_:_) _         = ImageStandalone
mkImageStatus c (Just (File _ raw_ts)) jpegs@(_:_) sidecar =
  if raw_ts' > jpeg_ts' + max_skew
    then ImageOutdated
    else ImageProcessed
  where raw_ts' = max raw_ts sidecar_ts
        sidecar_ts = maybe raw_ts fileMTime sidecar
        jpeg_ts' = minimum $ map fileMTime jpegs
        max_skew = cfgOutdatedError c

mkImage :: Config -> Text -> Text -> Maybe File
        -> Maybe File -> [File] -> Maybe (Text, Text) -> Image
mkImage config name parent raw sidecar jpeg range =
  Image name parent raw sidecar jpeg range $
  mkImageStatus config raw jpeg sidecar

data PicDir = PicDir
  { pdName      :: !Text
  , pdPaths     :: !([Text])
  , pdImages    :: !(Map.Map Text Image)
  , pdShadows   :: !(Map.Map Text Image)
  } deriving (Show)

type Repository = Map.Map Text PicDir

type FolderClassStats = Map.Map FolderClass Int

-- | Data type holding per-folder picture statistics.
data Stats = Stats
  { sRaw        :: !Int
  , sStandalone :: !Int
  , sProcessed  :: !Int
  , sOutdated   :: !Int
  , sOrphaned   :: !Int
  } deriving Show

-- | The empty (zero) stats.
zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0

-- | Data holding timeline stats.
type Timeline = Map.Map Day (Integer, Integer)

sumStats :: Stats -> Stats -> Stats
sumStats (Stats r1 s1 p1 o1 h1) (Stats r2 s2 p2 o2 h2) =
  Stats (r1 + r2) (s1 + s2) (p1 + p2) (o1 + o2) (h1 + h2)

updateStatsWithPic :: Stats -> Image -> Stats
updateStatsWithPic orig img =
  case imgStatus img of
    ImageRaw        -> orig { sRaw        = sRaw orig        + 1 }
    ImageStandalone -> orig { sStandalone = sStandalone orig + 1 }
    ImageProcessed  -> orig { sProcessed  = sProcessed orig  + 1 }
    ImageOutdated   -> orig { sOutdated   = sOutdated orig   + 1 }
    ImageOrphaned   -> orig { sOrphaned   = sOrphaned orig   + 1 }

computeFolderStats :: PicDir -> Stats
computeFolderStats =
  Map.foldl' updateStatsWithPic zeroStats . pdImages

data StrictPair a b = StrictPair !a !b

computeRepoStats :: Repository -> (Stats, FolderClassStats)
computeRepoStats =
  (\(StrictPair a b) -> (a, b)) .
  Map.foldl' (\(StrictPair picstats fcstats) dir ->
                let stats = computeFolderStats dir
                    fc = folderClassFromStats stats
                    picstats' = sumStats picstats stats
                    fcstats' = Map.insertWith (+) fc 1 fcstats
                in StrictPair picstats' fcstats'
             ) (StrictPair zeroStats Map.empty)

{-# NOINLINE repoCache #-}
repoCache :: MVar (Maybe Repository)
repoCache = unsafePerformIO $ newMVar Nothing

mergePictures :: Config -> Image -> Image -> Image
mergePictures c x y =
  let x' =
        x { imgRawPath     = imgRawPath     x `mplus` imgRawPath     y
          , imgSidecarPath = imgSidecarPath x `mplus` imgSidecarPath y
          , imgJpegPath    = imgJpegPath    x `mplus` imgJpegPath    y }
      status' = mkImageStatus c (imgRawPath x') (imgJpegPath x')
                  (imgSidecarPath x')
  in x' { imgStatus = status' }

mergeFolders :: Config -> PicDir -> PicDir -> PicDir
mergeFolders c x y =
  x { pdPaths = pdPaths x ++ pdPaths y
    , pdImages = Map.unionWith (mergePictures c) (pdImages x) (pdImages y)
    }

numRawPics :: PicDir -> Int
numRawPics = numPicsOfType (isJust . imgRawPath)

isUnprocessed :: Image -> Bool
isUnprocessed = (== ImageRaw ) . imgStatus

isProcessed :: Image -> Bool
isProcessed = (== ImageProcessed) . imgStatus

isOutdated :: Image -> Bool
isOutdated = (== ImageOutdated) . imgStatus

isOrphaned :: Image -> Bool
isOrphaned = (== ImageOrphaned) . imgStatus

numPics :: PicDir -> Int
numPics = Map.size . pdImages

numPicsOfType :: (Image -> Bool) -> PicDir -> Int
numPicsOfType criterion =
  Map.foldl go 0 . pdImages
    where go a i = if criterion i then a + 1 else a

numUnprocessedPics :: PicDir -> Int
numUnprocessedPics = numPicsOfType isUnprocessed

numProcessedPics :: PicDir -> Int
numProcessedPics = numPicsOfType isProcessed

isStandalone :: Image -> Bool
isStandalone = (== ImageStandalone) . imgStatus

numOutdatedPics :: PicDir -> Int
numOutdatedPics = numPicsOfType isOutdated

computeStandalonePics :: PicDir -> [Image]
computeStandalonePics =
  filter isStandalone . Map.elems . pdImages

numStandalonePics :: PicDir -> Int
numStandalonePics = numPicsOfType isStandalone

hasStandalonePics :: PicDir -> Bool
hasStandalonePics =
  not . null . computeStandalonePics

numOrphanedPics :: PicDir -> Int
numOrphanedPics = numPicsOfType isOrphaned

folderClass :: PicDir -> FolderClass
folderClass = folderClassFromStats . computeFolderStats

folderClassFromStats :: Stats -> FolderClass
folderClassFromStats stats@(Stats unproc standalone processed
                                  outdated orphaned) =
  let npics = unproc + standalone + processed + outdated + orphaned
      has_pics = npics /= 0
      has_unproc = unproc /= 0
      has_standalone = standalone /= 0
      all_unproc = unproc == npics
      has_raw = unproc /= 0 || processed /= 0 || outdated /= 0
      has_outdated = outdated /= 0
      has_orphaned = orphaned /= 0
      conditions = (has_pics, all_unproc, has_unproc,
                    has_standalone, has_raw, has_outdated, has_orphaned)
  in case conditions of
       -- folder with no pics is empty
       (False, True , False, False, False, False, False) -> FolderEmpty
       -- folder with all unprocessed is raw
       (True,  True , True , False, _    , False, False) -> FolderRaw
       -- folder with some unprocessed is unprocessed
       (True,  False, True , False, _    , _    , False) -> FolderUnprocessed
       -- folder with no raw files is standalone
       (True,  False, False, True , False, False, False) -> FolderStandalone
       -- folder with both standalone and some raw is mixed
       (True,  False, _    , True , True , _    , False) -> FolderMixed
       -- folder with orphaned pictures is mixed
       (True,  False, _    , _    , _    , _    , True ) -> FolderMixed
       -- folder with outdated pictures is outdated
       (True,  False, False, False, True , True , False) -> FolderOutdated
       -- othewise, folder is perfect - has only processed files
       (True,  False, False, False, True , False, False) -> FolderProcessed
       _ -> error $ "Wrong computation in folderClass: stats=" ++ show stats
                    ++ ", conditions=" ++ show conditions

getDirContents :: Config -> FilePath -> IO [(FilePath, FileStatus)]
getDirContents config base = do
  contents <- getDirectoryContents base
  let blkdirs = blacklistedDirs config
      allowed_names = filter (`notElem` blkdirs) contents
  mapM (\path -> do
           stat <- getSymbolicLinkStatus $ base </> path
           return (path, stat)
       ) allowed_names

scanDir :: Config
        -> Repository
        -> String
        -> IO Repository
scanDir config repo base = do
  paths <- getDirContents config base
  let dirs = filter (isDirectory . snd) paths
  foldM (\r p -> scanSubDir config r (base </> fst p)) repo dirs

scanSubDir :: Config
           -> Repository
           -> String
           -> IO Repository
scanSubDir config repository path = do
  allpaths <- getDirContents config path
  let dirpaths = filter (isDirectory . snd) allpaths
  let allpaths' = filter (isOKDir config) . map fst $ dirpaths
  foldM (\r s -> do
           dir <- loadDir config s (path </> s)
           return $ Map.insertWith (mergeFolders config) (pdName dir) dir r)
        repository allpaths'

recursiveScanDir :: Config -> FilePath -> IO [(FilePath, FileStatus)]
recursiveScanDir config base = do
  contents <- getDirContents config base
  let dirs = filter (isDirectory . snd) contents
  subdirs <- mapM (\s -> recursiveScanDir config (base </> fst s)) dirs
  return $ contents ++ concat subdirs

-- | Strict application of the 'Just' constructor. This is useful as
-- the Maybe type is not strict in its contained value.
strictJust :: a -> Maybe a
strictJust !a = Just a

addImgs :: Config -> Map.Map Text Image -> [Image] -> Map.Map Text Image
addImgs config =
  foldl' (\a i -> Map.insertWith (mergePictures config) (imgName i) i a)

loadDir :: Config -> String -> FilePath -> IO PicDir
loadDir config name path = do
  contents <- recursiveScanDir config path
  let rawe = rawExtsRev config
      side = sidecarExtsRev config
      jpeg = jpegExtsRev config
      tname = T.pack name
      loadImage (f, stat) =
        let base = dropCopySuffix config $ dropExtensions f
            tbase = T.pack base
            f' = reverse f
            tf = T.pack f
            jf = File tf mtime
            jtf = strictJust jf
            mtime = modificationTimeHiRes stat
            nfp = if hasExts f' rawe
                    then jtf
                    else Nothing
            sdc = if hasExts f' side
                    then jtf
                    else Nothing
            jpe = if hasExts f' jpeg
                    then [jf]
                    else []
            snames = expandRangeFile config base
            range = case snames of
                      [] -> Nothing
                      _ -> Just (T.pack $ head snames, T.pack $ last snames)
            img = mkImage config tbase tname nfp sdc jpe range
            simgs = map (\expname ->
                           mkImage config (T.pack expname) tname
                                   Nothing Nothing [jf] Nothing
                        ) snames
        in case (nfp, jpe, sdc) of
             (Nothing, [], Nothing) -> ([], [])
             -- no shadows for sidecar only files
             (Nothing, [], Just _) ->  ([img], [])
             _                      -> ([img], simgs)
      images = foldl' (\(imgs, shadows) f ->
                         let (newis, newss) = loadImage f
                         in (addImgs config imgs newis,
                             addImgs config shadows newss)
                      ) (Map.empty, Map.empty) contents
  return $ PicDir tname [T.pack path] (fst images) (snd images)


mergeShadows :: Config -> PicDir -> PicDir
mergeShadows config picd =
  let images' =
        Map.foldlWithKey (\imgs sname shadow ->
                            Map.adjust (\i -> mergePictures config i shadow)
                               sname imgs)
             (pdImages picd) (pdShadows picd)
  in picd { pdImages = images' }

maybeUpdateStandaloneRange :: Config -> PicDir -> Image -> Maybe Image
maybeUpdateStandaloneRange config picd img = do
  let iname = imgName img
  (begin, _) <- imgRange img
  _ <- if imgStatus img == ImageStandalone
         then Just ()
         else Nothing
  root <- Map.lookup begin (pdImages picd)
  _ <- imgRawPath root
  return $ mkImage config iname (imgParent img) (imgRawPath root)
           (imgSidecarPath img) (imgJpegPath img) (imgRange img)

resolveProcessedRanges :: Config -> PicDir -> PicDir
resolveProcessedRanges config picd =
  let images' =
        Map.foldl (\accimgs img ->
                     case maybeUpdateStandaloneRange  config picd img of
                       Nothing -> accimgs
                       Just img' -> Map.insert (imgName img') img' accimgs)
             (pdImages picd) (pdImages picd)
  in picd { pdImages = images' }


scanFilesystem :: Config -> IO Repository
scanFilesystem config = do
  repo <- foldM (\r d -> scanDir config r d) Map.empty $ cfgDirs config
  let repo' = Map.map (resolveProcessedRanges config .
                       mergeShadows config) repo
  return repo'

maybeUpdateCache :: Config
                 -> Maybe Repository
                 -> IO (Maybe Repository, Repository)
maybeUpdateCache config Nothing = do
  r <- scanFilesystem config
  return (Just r, r)
maybeUpdateCache _ orig@(Just r) = return (orig, r)

forceUpdateCache :: Config
                 -> Maybe Repository
                 -> IO (Maybe Repository, Repository)
forceUpdateCache config _ = maybeUpdateCache config Nothing

scanAll :: Config -> IO Repository
scanAll config =
  modifyMVar repoCache (maybeUpdateCache config)

forceScanAll :: Config -> IO Repository
forceScanAll config = modifyMVar repoCache (forceUpdateCache config)


computeStandaloneDirs :: Repository -> [PicDir]
computeStandaloneDirs =
  filter hasStandalonePics . Map.elems

filterDirsByClass :: [FolderClass] -> Repository -> [PicDir]
filterDirsByClass classes =
  filter ((`elem` classes) . folderClass) .
  Map.elems

dayFromTimestamp :: POSIXTime -> Day
dayFromTimestamp = utctDay . posixSecondsToUTCTime

sumPair :: (Num a) => (a, a) -> (a, a) -> (a, a)
sumPair (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

computeFolderTimeline :: Timeline -> PicDir -> Timeline
computeFolderTimeline timeline picdir = Map.foldl' (\t' img ->
    let jpeg_ts = case imgJpegPath img of
                    [] -> Nothing
                    j:_ -> strictJust $ fileMTime j
        ts_taken =
          case imgRawPath img of
            Just file -> Just $ fileMTime file
            Nothing -> jpeg_ts
        ts_processed =
          case imgRawPath img of
            Just _ -> jpeg_ts    -- if we have a raw file, whatever is
                                 -- the jpeg ts is our "processed" ts
                                 -- (possibly none)
            Nothing -> Nothing   -- if we don't have a raw file, we
                                 -- can't have a "processing"
                                 -- timestamp
        t'' = case ts_taken of
                Just ts -> Map.insertWith sumPair
                             (dayFromTimestamp ts) (1,0) t'
                Nothing -> t'
        t''' = case ts_processed of
                Just ts -> Map.insertWith sumPair
                             (dayFromTimestamp ts) (0,1) t''
                Nothing -> t''
    in t''') timeline (pdImages picdir)

computeTimeLine :: Repository -> Timeline
computeTimeLine = Map.foldl' computeFolderTimeline Map.empty
