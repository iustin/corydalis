{-# LANGUAGE TupleSections, OverloadedStrings, BangPatterns #-}

{-

Copyright (C) 2013 Iustin Pop

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

-}

module Pics ( PicDir(..)
            , Image(..)
            , Untracked(..)
            , File(..)
            , Repository
            , fileLastTouch
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
            , zeroStats
            , sumStats
            , totalStatsSize
            , totalStatsCount
            ) where

import Types

--import Import
import Prelude
import Control.Concurrent.MVar
import qualified Data.Text as T
import Data.Text (Text)
import System.IO.Unsafe

import Control.Monad
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import System.Directory
import System.FilePath
import System.Posix.Files hiding (fileSize)
import qualified System.Posix.Files (fileSize)
import System.Posix.Types
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
    [_:base:_] -> base
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
  , fileCTime :: !POSIXTime
  , fileMTime :: !POSIXTime
  , fileSize  :: !FileOffset
  , filePath  :: !Text
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

-- | A file with unknown (and untracked) type.
data Untracked = Untracked
  { untrkName   :: !Text
  , untrkParent :: !Text
  , untrkPaths  :: ![File]
  } deriving (Show)

-- | Return the laste time a file has been touched (latest of ctime/mtime).
fileLastTouch :: File -> POSIXTime
fileLastTouch f = fileMTime f `max` fileCTime f

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
mkImageStatus c (Just raw) jpegs@(_:_) sidecar =
  if raw_ts' > jpeg_ts' + max_skew
    then ImageOutdated
    else ImageProcessed
  where raw_ts = fileMTime raw
        raw_ts' = max raw_ts sidecar_ts
        sidecar_ts = maybe raw_ts fileMTime sidecar
        jpeg_ts' = minimum $ map fileLastTouch jpegs
        JSDiffTime max_skew = cfgOutdatedError c

mkImage :: Config -> Text -> Text -> Maybe File
        -> Maybe File -> [File] -> Maybe (Text, Text) -> Image
mkImage config name parent raw sidecar jpeg range =
  Image name parent raw sidecar jpeg range $
  mkImageStatus config raw jpeg sidecar

data PicDir = PicDir
  { pdName      :: !Text
  , pdMainPath  :: !Text
  , pdSecPaths  :: ![Text]
  , pdImages    :: !(Map.Map Text Image)
  , pdShadows   :: !(Map.Map Text Image)
  , pdUntracked :: !(Map.Map Text Untracked)
  } deriving (Show)

type Repository = Map.Map Text PicDir

type FolderClassStats = Map.Map FolderClass Int

-- | Data type holding per-folder picture statistics.
data Stats = Stats
  { sRaw            :: !Int
  , sStandalone     :: !Int
  , sProcessed      :: !Int
  , sOutdated       :: !Int
  , sOrphaned       :: !Int
  , sUntracked      :: !Int
  , sRawSize        :: !FileOffset
  , sProcSize       :: !FileOffset
  , sStandaloneSize :: !FileOffset
  , sSidecarSize    :: !FileOffset
  , sUntrackedSize  :: !FileOffset
  } deriving Show

-- | The empty (zero) stats.
zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0 0 0 0 0 0 0

-- | The total recorded size in a `Stats` structure.
totalStatsSize :: Stats -> FileOffset
totalStatsSize stats =
  sRawSize stats + sProcSize stats + sStandaloneSize stats +
           sSidecarSize stats + sUntrackedSize stats

-- | The total file count in a a `Stats` structure.
totalStatsCount :: Stats -> Int
totalStatsCount stats =
  sRaw stats + sStandalone stats + sProcessed stats + sOutdated stats
         + sOrphaned stats + sUntracked stats

-- | Data holding timeline stats.
type Timeline = Map.Map Day (Integer, Integer)

sumStats :: Stats -> Stats -> Stats
sumStats (Stats r1 s1 p1 o1 h1 u1 rs1 ps1 ss1 ms1 us1)
         (Stats r2 s2 p2 o2 h2 u2 rs2 ps2 ss2 ms2 us2) =
  Stats (r1 + r2) (s1 + s2) (p1 + p2) (o1 + o2) (h1 + h2) (u1 + u2)
        (rs1 + rs2) (ps1 + ps2) (ss1 + ss2) (ms1 + ms2) (us1 + us2)

updateStatsWithPic :: Stats -> Image -> Stats
updateStatsWithPic orig img =
  let status = imgStatus img
      stats = case status of
               ImageRaw        -> orig { sRaw        = sRaw orig        + 1 }
               ImageStandalone -> orig { sStandalone = sStandalone orig + 1 }
               ImageProcessed  -> orig { sProcessed  = sProcessed orig  + 1 }
               ImageOutdated   -> orig { sOutdated   = sOutdated orig   + 1 }
               ImageOrphaned   -> orig { sOrphaned   = sOrphaned orig   + 1 }
      rs = sRawSize stats
      rs' = case imgRawPath img of
              Nothing -> rs
              Just f -> rs + fileSize f
      jpeg_size = foldl' (\s f -> s + fileSize f) 0 (imgJpegPath img)
      ps = sProcSize stats
      ps' = if status == ImageProcessed ||
               status == ImageOutdated
              then ps + jpeg_size
              else ps
      ss = sStandaloneSize stats
      ss' = case status of
              ImageStandalone -> ss + jpeg_size
              _ -> ss
      ms = sSidecarSize stats
      ms' = ms + maybe 0 fileSize (imgSidecarPath img)
  in stats { sRawSize = rs'
           , sProcSize = ps'
           , sStandaloneSize = ss'
           , sSidecarSize = ms'
           }

updateStatsWithUntracked :: Stats -> Untracked -> Stats
updateStatsWithUntracked orig untrk =
  orig { sUntracked = sUntracked orig + 1
       , sUntrackedSize = sUntrackedSize orig + size}
    where
      size = foldl' (\s f -> s + fileSize f) 0 (untrkPaths untrk)

computeFolderStats :: PicDir -> Stats
computeFolderStats p =
  (Map.foldl' updateStatsWithUntracked `flip` pdUntracked p ) $
  Map.foldl' updateStatsWithPic zeroStats (pdImages p)

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

mergeUntracked :: Untracked -> Untracked -> Untracked
mergeUntracked x y =
  x { untrkPaths = untrkPaths x ++ untrkPaths y }

mergeFolders :: Config -> PicDir -> PicDir -> PicDir
mergeFolders c x y =
  x { pdMainPath = pdMainPath bestMainPath
    , pdSecPaths = pdSecPaths x ++ pdMainPath otherMainPath:pdSecPaths y
    , pdImages =
        Map.unionWith (mergePictures c) (pdImages x) (pdImages y)
    , pdUntracked =
        Map.unionWith mergeUntracked (pdUntracked x) (pdUntracked y)
    }
  where
    (bestMainPath, otherMainPath) =
      case (compare `on` (\z -> (numRawPics z, numPics z))) x y of
                                     GT -> (x, y)
                                     _ -> (y, x)


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
                                  outdated orphaned _ _ _ _ _ _) =
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
       -- pics all_u  has_u  has_s  has_r  has_ou has_or
       -- folder with no pics is empty
       (False, True , False, False, False, False, False) -> FolderEmpty
       -- folder with all unprocessed is raw
       (True,  True , True , False, _    , False, False) -> FolderRaw
       -- folder with some unprocessed (and maybe other types) is unprocessed
       (True,  False, True , _    , _    , _    , _    ) -> FolderUnprocessed
       -- folder with no raw files is standalone
       (True,  False, False, True , False, False, False) -> FolderStandalone
       -- folder with both standalone and some raw is mixed
       (True,  False, False, True , True , _    , False) -> FolderMixed
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

-- | Scans one of the directories defined in the configuration.
scanBaseDir :: Config
            -> Repository
            -> String
            -> IO Repository
scanBaseDir config repo base = do
  paths <- getDirContents config base
  let dirs = filter (isDirectory . snd) paths
  foldM (\r p -> scanSubDir config r (base </> fst p)) repo dirs

-- | Scans a directory one level below a base dir. The actual
-- subdirectory name is currently discarded and will not appear in the
-- final folder names.
scanSubDir :: Config
           -> Repository
           -> String
           -> IO Repository
scanSubDir config repository path = do
  allpaths <- getDirContents config path
  let dirpaths = filter (isDirectory . snd) allpaths
  let allpaths' = filter (isOKDir config) . map fst $ dirpaths
  foldM (\r s -> do
           dir <- loadFolder config s (path </> s)
           return $ Map.insertWith (mergeFolders config) (pdName dir) dir r)
        repository allpaths'

-- | Builds the filepath and filestatus pairs recursively for all
-- entries beneats a directory.
recursiveScanPath :: Config -> FilePath -> (FilePath -> FilePath)
                  -> IO [(FilePath, FileStatus)]
recursiveScanPath config base prepender = do
  contents <- getDirContents config base
  let (dirs, files) = partition (isDirectory . snd) contents
      dirs' = map fst dirs
      with_prefix = map (\(p, s) -> (prepender p, s)) files
  subdirs <- mapM (\p -> recursiveScanPath config (base </> p)
                           (prepender . (p </>))) dirs'
  return $ with_prefix ++ concat subdirs

-- | Strict application of the 'Just' constructor. This is useful as
-- the Maybe type is not strict in its contained value.
strictJust :: a -> Maybe a
strictJust !a = Just a

addImgs :: Config -> Map.Map Text Image -> [Image] -> Map.Map Text Image
addImgs config =
  foldl' (\a i -> Map.insertWith (mergePictures config) (imgName i) i a)

-- | Inserts a new other-type files into the others map.
addUntracked :: Map.Map Text Untracked -> [Untracked] -> Map.Map Text Untracked
addUntracked = foldl' (\a u -> Map.insertWith mergeUntracked (untrkName u) u a)

-- | Builds a `PicDir` (folder) from an entire filesystem subtree.
loadFolder :: Config -> String -> FilePath -> IO PicDir
loadFolder config name path = do
  contents <- recursiveScanPath config path id
  let rawe = rawExtsRev config
      side = sidecarExtsRev config
      jpeg = jpegExtsRev config
      tname = T.pack name
      loadImage (f, stat) =
        let base = dropCopySuffix config $ dropExtensions f
            tbase = T.pack base
            torig = T.pack f
            f' = reverse f
            tf = T.pack f
            jf = File tf ctime mtime size (T.pack $ path </> f)
            jtf = strictJust jf
            mtime = modificationTimeHiRes stat
            ctime = statusChangeTimeHiRes stat
            size = System.Posix.Files.fileSize stat
            nfp = if hasExts f' rawe
                    then jtf
                    else Nothing
            sdc = if hasExts f' side
                    then jtf
                    else Nothing
            is_jpeg = hasExts f' jpeg
            jpe = [jf | is_jpeg]
            snames = expandRangeFile config base
            range = case snames of
                      [] -> Nothing
                      _ -> Just (T.pack $ head snames, T.pack $ last snames)
            img = mkImage config tbase tname nfp sdc jpe range
            simgs = map (\expname ->
                           mkImage config (T.pack expname) tname
                                   Nothing Nothing [jf] Nothing
                        ) snames
            untrk = Untracked torig tname [jf]
        in case (nfp, jpe, sdc) of
             (Nothing, [], Nothing) -> ([], [], [untrk])
             -- no shadows for sidecar only files
             (Nothing, [], Just _) ->  ([img], [], [])
             _                      -> ([img], simgs, [])
      (images, shadows, untracked) =
        foldl' (\(images', shadows', untracked') f ->
                  let (newis, newss, newus) = loadImage f
                  in (addImgs config images' newis,
                      addImgs config shadows' newss,
                      addUntracked untracked' newus)
               ) (Map.empty, Map.empty, Map.empty) contents
  return $ PicDir tname (T.pack path) [] images shadows untracked


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
  repo <- foldM (scanBaseDir config) Map.empty $ cfgDirs config
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
