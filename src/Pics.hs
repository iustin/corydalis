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

{-# LANGUAGE TupleSections #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Pics ( PicDir(..)
            , Image(..)
            , Untracked(..)
            , File(..)
            , Flags(..)
            , Repository
            , Exif(..)
            , repoDirs
            , repoStats
            , repoExif
            , RepoStats(..)
            , ImageSize(..)
            , fileLastTouch
            , getRepo
            , scanAll
            , forceScanAll
            , isProcessed
            , isUnprocessed
            , isStandalone
            , folderClass
            , computeFolderStats
            , computeStandaloneDirs
            , numPics
            , numRawPics
            , numUnprocessedPics
            , numStandalonePics
            , numOrphanedPics
            , hasViewablePics
            , numProcessedPics
            , filterDirsByClass
            , filterImagesByClass
            , computeRepoStats
            , repoGlobalExif
            , computeTimeLine
            , Stats(..)
            , zeroStats
            , sumStats
            , totalStatsSize
            , totalStatsCount
            , loadCachedOrBuild
            , imageAtRes
            , allImageFiles
            , allRepoFiles
            ) where

import Types
import Cache
import Exif

import Prelude
import Control.Concurrent.MVar
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy.Encoding as T (decodeUtf8)
import qualified Data.Text.Lazy as T (toStrict)
import qualified Data.ByteString.Lazy as BSL (writeFile, length)
import System.IO.Unsafe
import Data.Int (Int64)
import Data.Time.LocalTime
import Data.Time.Calendar

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Concurrent (forkIO)
import Data.Default (def)
import Control.Exception
import Data.Function (on)
import qualified Data.Map.Strict as Map
import Data.List
import Data.Maybe
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Data.Semigroup
import System.Directory
import System.FilePath
import System.Posix.Files hiding (fileSize)
import qualified System.Posix.Files (fileSize)
import System.Posix.Types
import qualified Text.Regex.TDFA as TDFA
import System.Process.Typed
import System.IO.Error
import System.Exit

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
isOKDir cfg = TDFA.match (reRegex $ cfgDirRegex cfg)

dropCopySuffix :: Config -> String -> String
dropCopySuffix cfg name =
  case TDFA.match (reRegex $ cfgCopyRegex cfg) name of
    [_:base:_] -> base
    _ -> name

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                [(i, [])] -> Just i
                _   -> Nothing

expandRangeFile :: Config -> String -> [String]
expandRangeFile cfg name =
  case TDFA.match (reRegex $ cfgRangeRegex cfg) name of
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
  , fileExif  :: !(Maybe Exif)
  } deriving (Show)

instance NFData File where
  rnf File{..} = rnf fileName   `seq`
                 rnf fileCTime  `seq`
                 rnf fileMTime  `seq`
                 fileSize       `seq` -- plain type, weak form is enough
                 rnf filePath   `seq`
                 rnf fileExif

-- | Flags (on an image or a directory) showing exceptional
-- statuses.
data Flags = Flags
  { flagsSoftMaster :: !Bool
  } deriving (Show)

instance NFData Flags where
  rnf Flags{..} = rnf flagsSoftMaster

emptyFlags :: Flags
emptyFlags = Flags {
  flagsSoftMaster = False
  }

data Image = Image
    { imgName        :: !Text
    , imgParent      :: !Text
    , imgRawPath     :: !(Maybe File)
    , imgSidecarPath :: !(Maybe File)
    , imgJpegPath    :: ![File]
    , imgRange       :: !(Maybe (Text, Text))
    , imgExif        :: !(Maybe Exif)
    , imgStatus      :: !ImageStatus
    , imgFlags       :: !Flags
    } deriving (Show)

instance NFData Image where
  rnf Image{..} = rnf imgName        `seq`
                  rnf imgParent      `seq`
                  rnf imgRawPath     `seq`
                  rnf imgSidecarPath `seq`
                  rnf imgJpegPath    `seq`
                  rnf imgRange       `seq`
                  imgStatus          `seq` -- simple type
                  rnf imgFlags

-- | A file with unknown (and untracked) type.
data Untracked = Untracked
  { untrkName   :: !Text
  , untrkParent :: !Text
  , untrkPaths  :: ![File]
  } deriving (Show)

instance NFData Untracked where
  rnf Untracked{..} = rnf untrkName   `seq`
                      rnf untrkParent `seq`
                      rnf untrkPaths

data InodeInfo = InodeInfo
  { inodeName  :: !FilePath
  , inodeDir   :: !Bool
  , inodeMTime :: !POSIXTime
  , inodeCTime :: !POSIXTime
  , inodeSize  :: !FileOffset
  }

instance NFData InodeInfo where
  rnf InodeInfo{..} = rnf inodeName `seq`
                      rnf inodeDir  `seq`
                      inodeMTime    `seq`
                      inodeCTime    `seq`
                      inodeSize     `seq`
                      ()

-- | Represents a scaled down image size.
newtype ImageSize = ImageSize Int

-- | Return the laste time a file has been touched (latest of ctime/mtime).
fileLastTouch :: File -> POSIXTime
fileLastTouch f = fileMTime f `max` fileCTime f

-- | The (approximate) year of a file.
fileYear :: File -> Integer
fileYear f =
  let mintime = fileMTime f `min` fileCTime f
      utc = posixSecondsToUTCTime mintime
      days = utctDay utc
      (year, _, _) = toGregorian days
  in year

-- | The year of the image, as determined from Exif data.
imageYear :: Image -> Maybe Integer
imageYear img = do
  exif <- imgExif img
  date <- exifCreateDate exif
  let day = localDay date
      (y, _, _) = toGregorian day
  return y

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
mkImageStatus _ (Just _) jpegs@(_:_) _   = ImageProcessed

mkImage :: Config -> Text -> Text -> Maybe File
        -> Maybe File -> [File] -> Maybe (Text, Text) -> Flags -> Image
mkImage config name parent raw sidecar jpeg range =
  let status = mkImageStatus config raw jpeg sidecar
      exif = promoteFileExif (raw >>= fileExif)
               (sidecar >>= fileExif) (mapMaybe fileExif jpeg)
  in Image name parent raw sidecar jpeg range exif status

data PicDir = PicDir
  { pdName      :: !Text
  , pdMainPath  :: !Text
  , pdSecPaths  :: ![Text]
  , pdImages    :: !(Map.Map Text Image)
  , pdShadows   :: !(Map.Map Text Image)
  , pdUntracked :: !(Map.Map Text Untracked)
  , pdYear      :: !(Maybe Integer)  -- ^ The approximate year of the
                                     -- earliest picture.
  , pdExif      :: !GroupExif
  } deriving (Show)

instance NFData PicDir where
  rnf PicDir{..} = rnf pdName     `seq`
                   rnf pdMainPath `seq`
                   rnf pdSecPaths `seq`
                   rnf pdImages   `seq`
                   rnf pdShadows  `seq`
                   rnf pdUntracked `seq`
                   rnf pdYear      `seq`
                   rnf pdExif

type RepoDirs = Map.Map Text PicDir

data Repository = Repository
  { repoDirs  :: !RepoDirs
  , repoStats :: !RepoStats
  , repoExif  :: !GroupExif
  } deriving (Show)

instance NFData Repository where
  rnf Repository{..} = rnf repoDirs  `seq`
                       rnf repoStats `seq`
                       rnf repoExif

type FolderClassStats = Map.Map FolderClass Int

-- | Data type holding per-folder picture statistics.
data Stats = Stats
  { sRaw            :: !Int
  , sStandalone     :: !Int
  , sProcessed      :: !Int
  , sOrphaned       :: !Int
  , sUntracked      :: !Int
  , sRawSize        :: !FileOffset
  , sProcSize       :: !FileOffset
  , sStandaloneSize :: !FileOffset
  , sSidecarSize    :: !FileOffset
  , sUntrackedSize  :: !FileOffset
  , sByCamera       :: !(Map.Map Text (Int, FileOffset))
  , sByLens         :: !(Map.Map Text (Int, FileOffset))
  } deriving Show

-- orphan instance, sigh.
instance NFData COff where
  rnf (COff x) = rnf x

instance NFData Stats where
  rnf Stats{..} = rnf sByCamera `seq`
                  rnf sByLens

data RepoStats = RepoStats
  { rsPicStats :: !Stats
  , rsFCStats  :: !FolderClassStats
  } deriving (Show)

instance NFData RepoStats where
  rnf RepoStats{..} = rnf rsPicStats `seq`
                      rnf rsFCStats

-- | The empty (zero) stats.
zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0 0 0 0 0 0 Map.empty Map.empty

-- | The total recorded size in a `Stats` structure.
totalStatsSize :: Stats -> FileOffset
totalStatsSize stats =
  sRawSize stats + sProcSize stats + sStandaloneSize stats +
           sSidecarSize stats + sUntrackedSize stats

-- | The total file count in a a `Stats` structure.
totalStatsCount :: Stats -> Int
totalStatsCount stats =
  sRaw stats + sStandalone stats + sProcessed stats
         + sOrphaned stats + sUntracked stats

-- | Data holding timeline stats.
type Timeline = Map.Map Day (Integer, Integer)

sumTuple :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
sumTuple (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

sumStats :: Stats -> Stats -> Stats
sumStats (Stats r1 s1 p1 h1 u1 rs1 ps1 ss1 ms1 us1 sc1 sl1)
         (Stats r2 s2 p2 h2 u2 rs2 ps2 ss2 ms2 us2 sc2 sl2) =
  Stats (r1 + r2) (s1 + s2) (p1 + p2) (h1 + h2) (u1 + u2)
        (rs1 + rs2) (ps1 + ps2) (ss1 + ss2) (ms1 + ms2) (us1 + us2)
        sc sl
  where sc = Map.unionWith sumTuple sc1 sc2
        sl = Map.unionWith sumTuple sl1 sl2

updateStatsWithPic :: Stats -> Image -> Stats
updateStatsWithPic orig img =
  let status = imgStatus img
      stats = case status of
               ImageRaw        -> orig { sRaw        = sRaw orig        + 1 }
               ImageStandalone -> orig { sStandalone = sStandalone orig + 1 }
               ImageProcessed  -> orig { sProcessed  = sProcessed orig  + 1 }
               ImageOrphaned   -> orig { sOrphaned   = sOrphaned orig   + 1 }
      rs = sRawSize stats
      rs' = case imgRawPath img of
              Nothing -> rs
              Just f -> rs + fileSize f
      jpeg_size = foldl' (\s f -> s + fileSize f) 0 (imgJpegPath img)
      ps = sProcSize stats
      ps' = if status == ImageProcessed
              then ps + jpeg_size
              else ps
      ss = sStandaloneSize stats
      ss' = case status of
              ImageStandalone -> ss + jpeg_size
              _ -> ss
      ms = sSidecarSize stats
      ms' = ms + maybe 0 fileSize (imgSidecarPath img)
      camera = case imgRawPath img of
                 Nothing -> unknown
                 Just f -> fromMaybe unknown (exifCamera <$> fileExif f)
      xsize = case imgRawPath img of
               Just f -> fileSize f
               Nothing -> case imgJpegPath img of
                            x:_ -> fileSize x
                            _ -> 0
      lens = case imgRawPath img of
                 Nothing -> case imgJpegPath img of
                   j:_ -> fromMaybe unknown (exifLens <$> fileExif j)
                   _ -> unknown
                 Just f -> fromMaybe unknown (exifLens <$> fileExif f)
  in stats { sRawSize = rs'
           , sProcSize = ps'
           , sStandaloneSize = ss'
           , sSidecarSize = ms'
           , sByCamera = Map.insertWith sumTuple camera (1, xsize) (sByCamera orig)
           , sByLens = Map.insertWith sumTuple lens (1, xsize) (sByLens orig)
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

computeRepoStats :: RepoDirs -> RepoStats
computeRepoStats =
  (\(StrictPair a b) -> RepoStats a b) .
  Map.foldl' (\(StrictPair picstats fcstats) dir ->
                let stats = computeFolderStats dir
                    fc = folderClassFromStats stats
                    picstats' = sumStats picstats stats
                    fcstats' = Map.insertWith (+) fc 1 fcstats
                in StrictPair picstats' fcstats'
             ) (StrictPair zeroStats Map.empty)

repoGlobalExif :: RepoDirs -> GroupExif
repoGlobalExif =
  Map.foldl' (\e f -> e <> pdExif f) def

{-# NOINLINE repoCache #-}
repoCache :: MVar (Maybe Repository)
repoCache = unsafePerformIO $ newMVar Nothing

-- | Selects the best master file between two masters.
--
-- We select in the order of extensions as defined in the
-- config. Normally both files should have a raw extension, but we
-- should also handle the \"impossible\" cases: no matching extensions
-- for either file. If the extensions are the same, this will still
-- chew through the list. In both these cases, we (randomly) choose
-- the first file.
isBetterMaster :: [FilePath] -> FilePath -> FilePath -> Bool
isBetterMaster [] a b = True
isBetterMaster (e:es) a b | e == a = True
isBetterMaster (e:es) a b | e == b = False
isBetterMaster (e:es) a b | otherwise = isBetterMaster es a b

-- | Selects the best master file when merging images.
--
-- This simply chooses the first hard master file, and returns updated
-- soft master flag and potentially a demoted (usually soft) master to
-- jpeg. It can also be there are two of what we consider real master
-- files, in which case use the config order to select between
-- these. Usually one is really a derivation of the other.
selectMasterFile :: Config -> Image -> Image -> (Maybe File, Bool, [File])
selectMasterFile config x y =
  let xsoft = flagsSoftMaster $ imgFlags x
      ysoft = flagsSoftMaster $ imgFlags y
      xraw = imgRawPath x
      yraw = imgRawPath y
      rexts = cfgRawExts config
  in case (xraw, yraw) of
       (Just xf, Just yf)  -> case (xsoft, ysoft) of
                              -- Easy cases: only one is a real master.
                              (False, True)  -> (xraw, False, [yf])
                              (True, False)  -> (yraw, False, [xf])
                              -- Also easy: two soft masters, choose
                              -- either.
                              (True, True)   -> (xraw, True, [yf])
                              -- Hard case: two seemingly real
                              -- masters, let's choose in order of
                              -- priority.
                              (False, False) ->
                                let x_ext = drop 1 . takeExtension . T.unpack . fileName $ xf
                                    y_ext = drop 1 . takeExtension . T.unpack . fileName $ yf
                                in if isBetterMaster rexts x_ext y_ext
                                     then (xraw, False, [yf])
                                     else (yraw, False, [xf])
       (Just _, Nothing)   -> (xraw, xsoft, [])
       (Nothing, Just _)   -> (yraw, ysoft, [])
       _                   -> (Nothing, False, [])

mergePictures :: Config -> Image -> Image -> Image
mergePictures c x y =
  let (newraw, softmaster, extrajpeg) = selectMasterFile c x y
      x' =
        x { imgRawPath     = newraw
          , imgSidecarPath = imgSidecarPath x `mplus` imgSidecarPath y
          , imgJpegPath    = imgJpegPath    x `mplus` imgJpegPath    y
                             `mplus` extrajpeg
          , imgFlags = (imgFlags x) { flagsSoftMaster = softmaster }
          }
      status' = mkImageStatus c (imgRawPath x') (imgJpegPath x')
                  (imgSidecarPath x')
  in force $ x' { imgStatus = status' }

mergeUntracked :: Untracked -> Untracked -> Untracked
mergeUntracked x y =
  x { untrkPaths = untrkPaths x ++ untrkPaths y }

mergeFolders :: Config -> PicDir -> PicDir -> PicDir
mergeFolders c x y =
  force $
  x { pdMainPath = pdMainPath bestMainPath
    , pdSecPaths = pdSecPaths x ++ pdMainPath otherMainPath:pdSecPaths y
    , pdImages =
        Map.unionWith (mergePictures c) (pdImages x) (pdImages y)
    , pdUntracked =
        Map.unionWith mergeUntracked (pdUntracked x) (pdUntracked y)
    , pdYear = min <$> pdYear x <*> pdYear y <|>
               pdYear x <|>
               pdYear y
    , pdExif = pdExif x <> pdExif y
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

hasViewablePics :: PicDir -> Bool
hasViewablePics folder =
  numProcessedPics folder > 0 ||
  numStandalonePics folder > 0

folderClass :: PicDir -> FolderClass
folderClass = folderClassFromStats . computeFolderStats

folderClassFromStats :: Stats -> FolderClass
folderClassFromStats stats@(Stats unproc standalone processed
                                  orphaned _ _ _ _ _ _ _ _) =
  let npics = unproc + standalone + processed + orphaned
      has_pics = npics /= 0
      has_unproc = unproc /= 0
      has_standalone = standalone /= 0
      all_unproc = unproc == npics
      has_raw = unproc /= 0 || processed /= 0
      has_orphaned = orphaned /= 0
      conditions = (has_pics, all_unproc, has_unproc,
                    has_standalone, has_raw, has_orphaned)
  in case conditions of
       -- pics all_u  has_u  has_s  has_r  has_or
       -- folder with no pics is empty
       (False, True , False, False, False, False) -> FolderEmpty
       -- folder with all unprocessed is raw
       (True,  True , True , False, _    , False) -> FolderRaw
       -- folder with some unprocessed (and maybe other types) is unprocessed
       (True,  False, True , _    , _    , _    ) -> FolderUnprocessed
       -- folder with no raw files is standalone
       (True,  False, False, True , False, False) -> FolderStandalone
       -- folder with both standalone and some raw is mixed
       (True,  False, False, True , True , False) -> FolderMixed
       -- folder with orphaned pictures is mixed
       (True,  False, _    , _    , _    , True ) -> FolderMixed
       -- othewise, folder is perfect - has only processed files
       (True,  False, False, False, True , False) -> FolderProcessed
       _ -> error $ "Wrong computation in folderClass: stats=" ++ show stats
                    ++ ", conditions=" ++ show conditions

getDirContents :: Config -> FilePath -> IO ([FilePath], [InodeInfo])
getDirContents config base = do
  contents <- getDirectoryContents base
  let blkdirs = blacklistedDirs config
      allowed_names = filter (`notElem` blkdirs) contents
  paths <-
    foldM (\acc path -> do
             stat <- getSymbolicLinkStatus $ base </> path
             let !ii = InodeInfo { inodeName  = path
                                 , inodeDir   = isDirectory stat
                                 , inodeMTime = modificationTimeHiRes stat
                                 , inodeCTime = statusChangeTimeHiRes stat
                                 , inodeSize  = System.Posix.Files.fileSize stat
                                 }
             ii' <- evaluate $!! ii
             return $! ii':acc
       ) [] allowed_names
  let (dirs, files) = partition inodeDir paths
  return $!! (map inodeName dirs, files)

-- | Scans one of the directories defined in the configuration.
scanBaseDir :: Config
            -> RepoDirs
            -> String
            -> Bool
            -> IO RepoDirs
scanBaseDir config repo base isSource = do
  (dirs, _) <- getDirContents config base
  foldM (\r p -> scanSubDir config r (base </> p) isSource) repo dirs

-- | Scans a directory one level below a base dir. The actual
-- subdirectory name is currently discarded and will not appear in the
-- final folder names.
scanSubDir :: Config
           -> RepoDirs
           -> String
           -> Bool
           -> IO RepoDirs
scanSubDir config repository path isSource = do
  (dirpaths, _) <- getDirContents config path
  let allpaths' = filter (isOKDir config) dirpaths
  foldM (\r s -> do
            dir <- loadFolder config s (path </> s) isSource
            return $! Map.insertWith (mergeFolders config) (pdName dir) dir r)
    repository allpaths'

-- | Builds the filepath and filestatus pairs recursively for all
-- entries beneats a directory.
recursiveScanPath :: Config
                  -> FilePath
                  -> (FilePath -> FilePath)
                  -> IO [InodeInfo]
recursiveScanPath config base prepender = do
  (!dirs, !files) <- getDirContents config base
  let with_prefix = map (\ii -> ii { inodeName = prepender (inodeName ii) }) files
  subdirs <- mapM (\p -> recursiveScanPath config (base </> p)
                           (prepender . (p </>))) dirs
  return $ with_prefix ++ concat subdirs

-- | Strict application of the 'Just' constructor. This is useful as
-- the Maybe type is not strict in its contained value.
strictJust :: a -> Maybe a
strictJust !a = Just a

addImgs :: Config -> Map.Map Text Image -> [Image] -> Map.Map Text Image
addImgs config = foldl' (addImg config)

addImg :: Config -> Map.Map Text Image -> Image -> Map.Map Text Image
addImg config m i = Map.insertWith (mergePictures config) (imgName i) i m

-- | Inserts a new other-type files into the others map.
addUntracked :: Untracked -> Map.Map Text Untracked -> Map.Map Text Untracked
addUntracked u = Map.insertWith mergeUntracked (untrkName u) u

-- | Result of loadImage.
data LoadImageRes = LIRImage !Image ![Image]  -- ^ A single image with potential shadows.
                  | LIRUntracked !Untracked   -- ^ An untracked file.

-- | Builds a `PicDir` (folder) from an entire filesystem subtree.
loadFolder :: Config -> String -> FilePath -> Bool -> IO PicDir
loadFolder config name path isSource = do
  contents <- recursiveScanPath config path id
  lcache <- getExif config path $ map inodeName contents
  let rawe = rawExtsRev config
      side = sidecarExtsRev config
      jpeg = jpegExtsRev config
      tname = T.pack name
      loadImage ii  =
        let f = inodeName ii
            base = dropCopySuffix config $ dropExtensions f
            tbase = T.pack base
            torig = T.pack f
            f' = reverse f
            tf = T.pack f
            fullPath = T.pack $ path </> f
            exif = f `Map.lookup` lcache
            jf = File tf ctime mtime size fullPath exif
            jtf = strictJust jf
            mtime = inodeMTime ii
            ctime = inodeCTime ii
            size = inodeSize ii
            isSoftMaster = is_jpeg && isSource
            nfp = if hasExts f' rawe || isSoftMaster
                    then jtf
                    else Nothing
            sdc = if hasExts f' side
                    then jtf
                    else Nothing
            is_jpeg = hasExts f' jpeg
            jpe = [jf | is_jpeg && not isSource]
            snames = expandRangeFile config base
            range = case snames of
                      [] -> Nothing
                      _ -> Just (T.pack $ head snames, T.pack $ last snames)
            flags = Flags {
              flagsSoftMaster = isSoftMaster
              }
            simgs = map (\expname ->
                           mkImage config (T.pack expname) tname
                                   Nothing Nothing [jf] Nothing emptyFlags
                        ) snames
            untrk = Untracked torig tname [jf]
            onlySidecar = isNothing nfp && null jpe && isJust sdc
        in case (nfp, jpe, sdc) of
             (Nothing, [], Nothing)
               -> LIRUntracked untrk
             -- no shadows for sidecar only files
             _ -> LIRImage img (if onlySidecar then [] else simgs)
                    where img = force $ mkImage config tbase tname nfp sdc jpe range flags
      (images, shadows, untracked) =
        foldl' (\(images', shadows', untracked') f ->
                  case loadImage f of
                    LIRImage img newss -> (addImg config images' img,
                                           addImgs config shadows' newss,
                                           untracked')
                    LIRUntracked untrk -> (images', shadows',
                                           addUntracked untrk untracked')
               ) (Map.empty, Map.empty, Map.empty) contents
  let year = Map.foldl' (\a img ->
                           (min <$> a <*> imageYear img) <|>
                           a <|>
                           imageYear img
                        ) Nothing images
      exif = Map.foldl' (\e img -> case imgExif img of
                                     Just ie -> addExifToGroup e ie
                                     Nothing -> e
                        ) def images
  return $!! PicDir tname (T.pack path) [] images shadows untracked year exif


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
           (imgFlags img)

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
  let srcdirs = zip (cfgSourceDirs config) (repeat True)
      outdirs = zip (cfgOutputDirs config) (repeat False)
  repo <- foldM (\repo (dir,issrc) -> scanBaseDir config repo dir issrc)
            Map.empty $ srcdirs ++ outdirs
  let repo' = Map.map (resolveProcessedRanges config .
                       mergeShadows config) repo
      stats = computeRepoStats repo'
      rexif = repoGlobalExif repo'
      repo'' = Repository { repoDirs  = repo'
                          , repoStats = stats
                          , repoExif  = rexif
                          }
  forkIO $ forceBuildThumbCaches config repo''
  return $!! repo''

forceBuildThumbCaches :: Config -> Repository -> IO ()
forceBuildThumbCaches config repo = do
  let images = filterImagesByClass [ImageProcessed, ImageStandalone] repo
  mapM_ (\i ->
           case imgJpegPath i of
             f:_ -> mapM_ (loadCachedOrBuild config (filePath f) Nothing (fileLastTouch f))
                     (map ImageSize (cfgAutoImageSizes config))
             _ -> return ()
        ) images

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

-- | Tries to cheaply return the repository.
--
-- If we already have a repository, return it. Otherwise, signal no
-- repository, so that `scanAll` can be called for the expensive
-- scan. This function thus allows a cheap retrieve without having to
-- scan the database for exif information pre-caching, etc.
getRepo :: IO (Maybe Repository)
getRepo = readMVar repoCache

scanAll :: Config -> IO Repository
scanAll config =
  modifyMVar repoCache (maybeUpdateCache config)

forceScanAll :: Config -> IO Repository
forceScanAll config = modifyMVar repoCache (forceUpdateCache config)

computeStandaloneDirs :: Repository -> [PicDir]
computeStandaloneDirs =
  filter hasStandalonePics . Map.elems . repoDirs

filterDirsByClass :: [FolderClass] -> Repository -> [PicDir]
filterDirsByClass classes =
  filter ((`elem` classes) . folderClass) .
  Map.elems . repoDirs

filterImagesByClass :: [ImageStatus] -> Repository -> [Image]
filterImagesByClass classes =
  foldl' (\pics folder ->
           let folderPics = filter (\p -> imgStatus p `elem` classes) .
                            Map.elems . pdImages $ folder
           in pics ++ folderPics) [] . Map.elems . repoDirs

allImageFiles :: Image -> [File]
allImageFiles img =
  concat [ maybeToList $ imgRawPath img
         , maybeToList $ imgSidecarPath img
         , imgJpegPath img
         ]

addDirFiles :: PicDir -> [File] -> [File]
addDirFiles dir =
  flip (Map.foldl' (\fs untrk -> untrkPaths untrk ++ fs)) (pdUntracked dir) .
  flip (Map.foldl' (\fs img -> allImageFiles img ++ fs)) (pdImages dir)

allRepoFiles :: Repository -> [File]
allRepoFiles =
  Map.foldl' (flip addDirFiles) [] . repoDirs

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
computeTimeLine = Map.foldl' computeFolderTimeline Map.empty . repoDirs

-- | Computes optimal image preview size.
--
-- Assuming the passed list is sorted, the function computes the
-- smallest size that is larger or equal than the required image
-- size. If none is found, it means we're requesting a size larger
-- than any of our allowed-to-be-cached sizes, so return Nothing.
findBestSize :: ImageSize -> [Int] -> Maybe Int
findBestSize _ [] = Nothing
findBestSize r@(ImageSize s) (x:xs) =
  if s <= x
    then Just x
    else findBestSize r xs

embeddedImagePath :: Config -> FilePath -> String
embeddedImagePath config path =
  cachedBasename config path "embedded"

scaledImagePath :: Config -> FilePath -> Int -> String
scaledImagePath config path res =
  cachedBasename config path (show res)

-- | Generate a preview for an image.
--
-- In this context, a preview is a smaller version of an image, for
-- faster/optimised use on a lower-powered device (mobile, etc.). It
-- is done using imagemagick's convert tool.
--
-- TODO: add loging of failures.
-- TODO: fix the issue that range files and their components have identical output.
-- TODO: improve path manipulation \/ concatenation.
-- TODO: for images smaller than given source, we generate redundant previews.
-- TODO: stop presuming all images are jpeg.
loadCachedOrBuild :: Config -> Text -> Maybe Text -> POSIXTime -> ImageSize -> IO (Text, Text)
loadCachedOrBuild config origPath srcPath mtime size = do
  let bytesPath = fromMaybe origPath srcPath
      res = findBestSize size (cfgAllImageSizes config)
  case res of
    Nothing -> return ("image/jpeg", bytesPath)
    Just res' -> do
      let geom = show res' ++ "x" ++ show res'
          fpath = scaledImagePath config (T.unpack origPath) res'
          isThumb = res' <= cfgThumbnailSize config
          format = if isThumb then "png" else "jpg"
      stat <- tryJust (guard . isDoesNotExistError) $ getFileStatus fpath
      let needsGen = case stat of
                       Left _ -> True
                       Right st -> modificationTimeHiRes st < mtime
      when needsGen $ do
        let operators = if isThumb
                          then ["-thumbnail", geom, "-background", "none", "-gravity", "center", "-extent", geom]
                          else ["-resize", geom]
            (parent, _) = splitFileName fpath
            outFile = format ++ ":" ++ fpath
        createDirectoryIfMissing True parent
        (exitCode, out, err) <- readProcess $ proc "convert" (concat [[T.unpack bytesPath], operators, [outFile]])
        return ()
      return $ (T.pack $ "image/" ++ format, T.pack fpath)

-- | Ordered list of tags that represent embedded images.
previewTags :: [String]
previewTags = ["JpgFromRaw", "PreviewImage"]

-- | Minimum thumbnail size to consider it a valid image.
minImageSize :: Int64
minImageSize = 512

-- | Extracts and saves an embedded thumbnail from an image.
--
-- The extract image type is presumed (and required) to be jpeg.
extractEmbedded :: Config -> Text -> String -> IO (Either Text Text)
extractEmbedded config path tag = do
  let outputPath = embeddedImagePath config (T.unpack path)
      (outputDir, _) = splitFileName outputPath
      args = [
        "-b",
        "-" ++ tag,
        T.unpack path
        ]
  exists <- fileExist outputPath
  if not exists
    then do
      -- TODO: write directly to temp file via setStdOut +
      -- useHandleOpen + ..., then rename atomically if successful.
      let pconfig =
            setStdin closed
            . setCloseFds True
            $ proc "exiftool" args
      (exitCode, out, err) <- readProcess pconfig
      if exitCode == ExitSuccess && BSL.length out >= minImageSize
        then do
          createDirectoryIfMissing True outputDir
          BSL.writeFile outputPath out
          return $ Right $ T.pack outputPath
        else
          return $ Left $ T.toStrict $ T.decodeUtf8 err -- partial function!
    else
      return $ Right $ T.pack outputPath

-- | Extract the first valid thumbnail from an image.
bestEmbedded :: Config -> Text -> IO (Either Text Text)
bestEmbedded config path =
  go config path previewTags
  where go _ _ [] = return $ Left "No tags available"
        go c p (x:xs) = do
          r <- extractEmbedded c p x
          case r of
            Left _ -> case xs of
              [] -> return r
              _ -> go c p xs
            Right _ -> return r

-- | Return the path (on the filesystem) to a specific size rendering of an image.
--
-- If there was no specific size requested, return the path to an
-- unscaled variant. If a size was requested, return the smallest
-- rendering at a size larger-or-equal than the requested one.
--
-- In case the image requested doesn't already have a valid jpeg
-- variant, try to extract an embedded preview from one of the raw
-- files, and use that as substitute full-size version, before scaling
-- down (as needed).
--
-- TODO: handle and meaningfully return errors.
imageAtRes :: Config -> Image -> Maybe ImageSize -> IO (Text, Text)
imageAtRes config img size = do
  (raw, origFile) <- case imgJpegPath img of
                       j:_ -> return (False, j)
                       _   -> case (imgRawPath img, flagsSoftMaster (imgFlags img)) of
                         (Just r, True) -> return (False, r)
                         (Just r, False) -> return (True, r)
                         _ -> error "Neither raw nor jpeg image available"
  srcPath <- if raw
               then do
                 thumb <- bestEmbedded config (filePath origFile)
                 case thumb of
                   Left err -> error $ "Error while extracting embedded image: " ++ T.unpack err
                   Right path -> return $ Just path
               else return Nothing
  case size of
    -- TODO: don't use hardcoded jpeg type!
    Nothing -> return ("image/jpeg", fromMaybe (filePath origFile) srcPath)
    Just s -> loadCachedOrBuild config (filePath origFile) srcPath (fileLastTouch origFile) s
