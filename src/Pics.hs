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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Pics ( PicDir(..)
            , Image(..)
            , ImageTimeKey
            , ImageError(..)
            , MediaType(..)
            , MimeType
            , File(..)
            , filePath
            , Flags(..)
            , Repository
            , Ctx
            , initContext
            , Exif(..)
            , RepoDirs
            , repoDirs
            , repoStats
            , repoExif
            , repoStatus
            , repoSerial
            , RepoStats(..)
            , RepoStatus(..)
            , ImageSize(..)
            , FileOffset
            , Occurrence(..)
            , Trends
            , CameraInfo(..)
            , fileLastTouch
            , fileMimeType
            , getRepo
            , getProgress
            , getRenderProgress
            , getCleanProgress
            , scanAll
            , launchScanFileSystem
            , waitForScan
            , folderClass
            , folderClassFromStats
            , computeFolderStats
            , numPics
            , numRawPics
            , numUnprocessedPics
            , numStandalonePics
            , hasViewablePics
            , numProcessedPics
            , filterImagesByClass
            , filterImagesBy
            , filterFoldersBy
            , computeRepoStats
            , repoGlobalExif
            , Stats(..)
            , zeroStats
            , sumStats
            , totalStatsSize
            , totalStatsCount
            , loadCachedOrBuild
            , imageAtRes
            , allImageFiles
            , allViewableImageFiles
            , allRepoFiles
            , imageHasImages
            , imageHasMovies
            , imageHasUntracked
            , imageYear
            , imageYearMonth
            , imageTimeKey
            , SearchResults
            , SearchResultsPics
            , getSearchResults
            , SearchCache
            , imgProblems
            , pdProblems
            , repoProblems
            , transformForFile
            , transformForImage
            , updateStatsWithPic
            -- Test export :/
            , mkImage
            ) where

import           Control.Applicative
import           Control.DeepSeq
import qualified Data.ByteString.Lazy    as BSL (append, length, writeFile)
import qualified Data.Foldable
import           Data.LruCache           (LruCache)
import qualified Data.LruCache           as LRU
import qualified Data.Map.Strict         as Map
import           Data.Semigroup
import qualified Data.Set                as Set
import qualified Data.Store
import           Data.Store.TH
import qualified Data.Text               as Text
import qualified Data.Text.Lazy          as TextL
import qualified Data.Text.Lazy.Encoding as Text (decodeUtf8)
import qualified Data.Text.Read          as Text
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Error
import           System.Log.FastLogger   (toLogStr)
import qualified System.Posix.Files      (fileSize)
import           System.Posix.Files      hiding (fileSize)
import           System.Posix.Types
import           System.Process.Typed
import qualified Text.Regex.TDFA         as TDFA
import           UnliftIO.Exception

import           Cache
import           Compat.Orphans          ()
import           Exif
import           Import.NoFoundation     hiding (fileName, fileSize)
import           Stats                   (CameraInfo (..), DateRange,
                                          Occurrence (..), Trends,
                                          mergeMinMaxPair, ocFromSize)

type Ctx = Context Repository SearchCache

-- TODO: move this to IO, so that callers don't have to know about
-- atomically? [cleanup]
initContext :: Config -> LogFn -> STM Ctx
initContext config logfn =
  newContext config logfn startupRepository emptySearchCache

neverRecurseDirs :: [String]
neverRecurseDirs = [".", ".."]

blacklistedDirs :: Config -> [String]
blacklistedDirs config = neverRecurseDirs ++ cfgBlacklistedDirs config

isOKDir :: Config -> String -> Bool
isOKDir cfg = TDFA.match (reRegex $ cfgDirRegex cfg)

dropCopySuffix :: Config -> String -> String
dropCopySuffix cfg name =
  case TDFA.match (reRegex $ cfgCopyRegex cfg) name of
    [_:b:_] -> b
    _       -> name

-- | Makes a path "absolute" by dropping prefix slashes.
makeRel :: FilePath -> FilePath
makeRel = dropWhile (== pathSeparator)

-- FIXME: duplication with Indexer.
parseDecimal :: (Integral a) => String -> Maybe a
parseDecimal (Text.pack -> w) =
  case Text.decimal w of
    Right (w', "") -> Just w'
    _              -> Nothing

expandRangeFile :: Config -> String -> [String]
expandRangeFile cfg name =
  case TDFA.match (reRegex $ cfgRangeRegex cfg) name of
    [[_, root, begin, end]] -> let ib = parseDecimal begin::Maybe Int
                                   ie = parseDecimal end
                                   expand s = if length s >= length begin
                                                then s
                                                else expand ('0':s)
                               in case (ib, ie) of
                                    (Just b, Just e) ->
                                      [root ++ expand (show i) | i <- [b..e]]
                                    _ -> []
    _ -> []

-- Readability alias.
type MimeType = Text

data File = File
  { fileName  :: !Text
  , fileCTime :: !POSIXTime
  , fileMTime :: !POSIXTime
  , fileSize  :: !FileOffset
  , fileDir   :: !Text
  , fileExif  :: !Exif
  } deriving (Show, Eq)

instance NFData File where
  rnf File{..} = rnf fileName   `seq`
                 rnf fileCTime  `seq`
                 rnf fileMTime  `seq`
                 fileSize       `seq` -- plain type, weak form is enough
                 rnf fileDir    `seq`
                 rnf fileExif

-- | The full path for a file.
filePath :: File -> LazyText
filePath File{..} = TextL.fromChunks [fileDir, pathSep, fileName]

-- | Try to find a valid mime type for a file.
fileMimeType :: Text -> File -> Text
fileMimeType d = fromMaybe d . exifMimeType . fileExif

-- | Flags (on an image or a directory) showing exceptional
-- statuses.
{-# ANN Flags ("HLint: ignore Use newtype instead of data"::String) #-}
data Flags = Flags
  { flagsSoftMaster :: !Bool
  } deriving (Show, Eq)

instance NFData Flags where
  rnf Flags{..} = rnf flagsSoftMaster

instance Default Flags where
  def = emptyFlags

emptyFlags :: Flags
emptyFlags = Flags {
  flagsSoftMaster = False
  }

-- | Media type for an image.
--
-- Note the order of constructors matter, as we rely on the Ord
-- instance for promotion.
data MediaType = MediaUnknown
               | MediaImage
               | MediaMovie
               deriving (Show, Eq, Ord)

instance NFData MediaType where
  rnf _ = ()

data Image = Image
    { imgName        :: !ImageName
    , imgParent      :: !Text
    , imgRawPath     :: !(Maybe File)
    , imgSidecarPath :: !(Maybe File)
    , imgJpegPath    :: ![File]
    , imgMasterMov   :: !(Maybe File)
    , imgMovs        :: ![File]
    , imgUntracked   :: ![File]
    , imgRange       :: !(Maybe (ImageName, ImageName))
    , imgExif        :: !Exif
    , imgType        :: !MediaType
    , imgStatus      :: !ImageStatus
    , imgFlags       :: !Flags
    } deriving (Show, Eq)

instance NFData Image where
  rnf Image{..} = rnf imgName        `seq`
                  rnf imgParent      `seq`
                  rnf imgRawPath     `seq`
                  rnf imgSidecarPath `seq`
                  rnf imgJpegPath    `seq`
                  rnf imgRange       `seq`
                  rnf imgType        `seq`
                  rnf imgStatus      `seq`
                  rnf imgFlags

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

data ImageError = ImageNotViewable
                | ImageError Text
  deriving (Show, Typeable)

instance Exception ImageError

-- | Represents a scaled down image size.
newtype ImageSize = ImageSize Int

-- | Return the last time a file has been touched (latest of ctime/mtime).
fileLastTouch :: File -> POSIXTime
fileLastTouch f = fileMTime f `max` fileCTime f

-- | Retun the last time a path on disk has been touched.
filePathLastTouch :: FilePath -> IO POSIXTime
filePathLastTouch p = do
  stat <- getSymbolicLinkStatus p
  return $ modificationTimeHiRes stat `max` statusChangeTimeHiRes stat

-- | The year of the image, as determined from Exif data.
imageYear :: Image -> Maybe Integer
imageYear img = do
  let exif = imgExif img
  date <- exifLocalCreateDate exif
  let day = localDay  date
      (y, _, _) = toGregorian day
  return y

-- | The year of the image, as determined from Exif data.
imageYearMonth :: Image -> Maybe (Int, Int)
imageYearMonth img = do
  let exif = imgExif img
  date <- exifLocalCreateDate exif
  let day = localDay date
      (y, m, _) = toGregorian day
  -- Let's hope the year/month stay sane here.
  return (fromIntegral y, fromIntegral m)

imageLocalDate :: Image -> Maybe LocalTime
imageLocalDate = exifLocalCreateDate . imgExif

type ImageTimeKey = (Maybe LocalTime, ImageName)

imageTimeKey :: Image -> ImageTimeKey
imageTimeKey img = (imageLocalDate img, imgName img)

-- | Computes the status of an image given the files that back it
-- (raw, jpeg, sidecar).
mkImageStatus :: Config
              -> Maybe File  -- ^ Raw file
              -> [File]      -- ^ Jpeg file(s)
              -> Maybe File  -- ^ Sidecar file
              -> Bool        -- ^ Has movies or untracked files
              -> ImageStatus
mkImageStatus _ Nothing  []    Nothing   False =
  error "imageStatus - neither raw nor standalone nor orphaned nor movies"
mkImageStatus _ Nothing  []    Nothing   True  = ImageProcessed
mkImageStatus _ Nothing  []    (Just _)  _     = ImageOrphaned
mkImageStatus _ Nothing  (_:_) (Just _)  _     = ImageStandalone
  --error "imageStatus - orphaned + jpeg?"
mkImageStatus _ (Just _) []    _         _     = ImageUnprocessed
mkImageStatus _ Nothing  (_:_) _         _     = ImageStandalone
mkImageStatus _ (Just _) (_:_) _         _     = ImageProcessed

mkImage :: Config
        -> ImageName          -- ^ Name
        -> Text               -- ^ Parent
        -> Maybe File         -- ^ Raw path
        -> Maybe File         -- ^ Sidecar path
        -> [File]             -- ^ Jpegs
        -> Maybe File         -- ^ Master movie
        -> [File]             -- ^ Movies
        -> [File]             -- ^ Untracked files
        -> Maybe (ImageName, ImageName) -- ^ Range
        -> MediaType          -- ^ Media type
        -> Flags              -- ^ Flags
        -> Image
mkImage config name parent raw sidecar jpegs mmov movs untrk range mtype =
  let status = mkImageStatus config raw jpegs sidecar
                 (isJust mmov || not (null movs) || not (null untrk))
      exif = promoteFileExif
               (fileExif <$> raw)
               (fileExif <$> sidecar)
               (map fileExif jpegs)
               (fileExif <$> mmov)
               (map fileExif movs)
  in Image name parent raw sidecar jpegs mmov movs untrk range exif mtype status

data PicDir = PicDir
  { pdName     :: !Text
  , pdMainPath :: !Text
  , pdSecPaths :: ![Text]
  , pdImages   :: !(Map ImageName Image)
  , pdTimeSort :: !(Set ImageTimeKey)
  , pdShadows  :: !(Map ImageName Image)
  , pdYear     :: !(Maybe Integer)  -- ^ The approximate year of the
                                    -- earliest picture.
  , pdExif     :: !GroupExif
  , pdStats    :: !Stats
  } deriving (Show)

instance NFData PicDir where
  rnf PicDir{..} = rnf pdName     `seq`
                   rnf pdMainPath `seq`
                   rnf pdSecPaths `seq`
                   rnf pdImages   `seq`
                   rnf pdShadows  `seq`
                   rnf pdYear     `seq`
                   rnf pdExif     `seq`
                   rnf pdStats

type RepoDirs = Map Text PicDir

-- | Status of a repository.
data RepoStatus = RepoEmpty    -- ^ Only to be used at application
                               -- startup time!
                | RepoStarting -- ^ Denotes an empty, in-process of
                               -- being rescanned repo.
                | RepoScanning
                  { rsScanGoal :: !WorkStart
                  }
                | RepoRendering
                  { rsScanResults :: !WorkResults
                  , rsRenderGoal  :: !WorkStart
                  }
                | RepoCleaning
                  { rsScanResults   :: !WorkResults
                  , rsRenderResults :: !WorkResults
                  , rsCleanGoal     :: !WorkStart
                  }
                | RepoFinished
                  { rsScanResults   :: !WorkResults
                  , rsRenderResults :: !WorkResults
                  , rsCleanResults  :: !WorkResults
                  }
                | RepoError !Text
  deriving (Show)

instance NFData RepoStatus where
  rnf RepoEmpty            = ()
  rnf RepoStarting         = ()
  rnf (RepoScanning ws)    = rnf ws
  rnf (RepoRendering s r)  = rnf s `seq` rnf r
  rnf (RepoCleaning s r c) = rnf s `seq` rnf r `seq` rnf c
  rnf (RepoFinished s r c) = rnf s `seq` rnf r `seq` rnf c
  rnf (RepoError t)        = rnf t

instance Default RepoStatus where
  def = RepoEmpty

data Repository = Repository
  { repoDirs   :: !RepoDirs
  , repoStats  :: !RepoStats
  , repoExif   :: !GroupExif
  , repoStatus :: !RepoStatus
  , repoSerial :: !Int
  } deriving (Show)

instance NFData Repository where
  rnf Repository{..} = rnf repoDirs   `seq`
                       rnf repoStats  `seq`
                       rnf repoExif   `seq`
                       rnf repoStatus `seq`
                       rnf repoSerial

-- | Value to be set at application startup time.
--
-- This will allow differentiation from "in progess of rescanning".
startupRepository :: Repository
startupRepository =
   Repository { repoDirs   = Map.empty
              , repoStats  = def
              , repoExif   = def
              , repoStatus = def
              , repoSerial = 0
              }

-- | Changes an existing repository into starting state.
rescanningRepository :: Repository -> Repository
rescanningRepository repo = repo { repoStatus = RepoStarting }

type FolderClassStats = Map FolderClass Int

-- | Data type holding per-folder picture statistics.
data Stats = Stats
  { sRaw            :: !Int
  , sStandalone     :: !Int
  , sProcessed      :: !Int
  , sOrphaned       :: !Int
  , sUntracked      :: !Int
  , sMovies         :: !Int
  , sRawSize        :: !FileOffset
  , sProcSize       :: !FileOffset
  , sStandaloneSize :: !FileOffset
  , sSidecarSize    :: !FileOffset
  , sUntrackedSize  :: !FileOffset
  , sMovieSize      :: !FileOffset
  , sByCamera       :: !(Map Text (Occurrence CameraInfo))
  , sByLens         :: !(Map Text (Occurrence LensInfo))
  , sPeople         :: !(Set Text)
  , sDateRange      :: !(Maybe DateRange)
  } deriving Show

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

instance Default RepoStats where
  def = RepoStats def def


-- | Type alias for image search results, weakly capture-time-sorted.
type SearchResultsPics = Map (Text, ImageTimeKey) Image

-- | Overall search results type alias.
type SearchResults = (SearchResultsPics, Map Text Image)

-- | Type of the search cache.
type SearchCache = LruCache UrlParams SearchResults

-- TODO: replace hardcoded cache size with config option.
emptySearchCache :: SearchCache
emptySearchCache = LRU.empty 10

-- | The empty (zero) stats.
zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 Map.empty Map.empty Set.empty Nothing

instance Default Stats where
  def = zeroStats

$(makeStore ''File)
$(makeStore ''MediaType)
$(makeStore ''Flags)
$(makeStore ''Image)
$(makeStore ''Stats)
$(makeStore ''RepoStats)
$(makeStore ''PicDir)
$(makeStore ''RepoStatus)
$(makeStore ''Repository)

-- | The total recorded size in a `Stats` structure.
totalStatsSize :: Stats -> FileOffset
totalStatsSize stats =
  sRawSize stats + sProcSize stats + sStandaloneSize stats +
           sSidecarSize stats + sUntrackedSize stats +
           sMovieSize stats

-- | The total file count in a a `Stats` structure.
totalStatsCount :: Stats -> Int
totalStatsCount stats =
  sRaw stats + sStandalone stats + sProcessed stats
         + sOrphaned stats + sUntracked stats
         + sMovies stats

sumStats :: Stats -> Stats -> Stats
sumStats (Stats r1 s1 p1 h1 u1 m1 rs1 ps1 ss1 sd1 us1 ms1 sc1 sl1 sp1 sdr1)
         (Stats r2 s2 p2 h2 u2 m2 rs2 ps2 ss2 sd2 us2 ms2 sc2 sl2 sp2 sdr2) =
  Stats (r1 + r2) (s1 + s2) (p1 + p2) (h1 + h2) (u1 + u2) (m1 + m2)
        (rs1 + rs2) (ps1 + ps2) (ss1 + ss2) (sd1 + sd2) (us1 + us2) (ms1 + ms2)
        sc sl sp sdr

  where sc = Map.unionWith mappend sc1 sc2
        sl = Map.unionWith (<>) sl1 sl2
        sp = Set.union sp1 sp2
        sdr = mergeMinMaxPair sdr1 sdr2

updateStatsWithPic :: Stats -> Image -> Stats
updateStatsWithPic orig img =
  let status = imgStatus img
      stats = case imgType img of
        MediaMovie   -> orig { sMovies    = sMovies orig    + 1 }
        MediaUnknown -> orig { sUntracked = sUntracked orig + 1 }
        MediaImage   -> case status of
          ImageUnprocessed -> orig { sRaw        = sRaw orig        + 1 }
          ImageStandalone  -> orig { sStandalone = sStandalone orig + 1 }
          ImageProcessed   -> orig { sProcessed  = sProcessed orig  + 1 }
          ImageOrphaned    -> orig { sOrphaned   = sOrphaned orig   + 1 }
      rs = sRawSize stats
      rs' = case imgRawPath img of
              Nothing -> rs
              Just f  -> rs + fileSize f
      jpeg_size = foldl' (\s f -> s + fileSize f) 0 (imgJpegPath img)
      ps = sProcSize stats
      ps' = if status == ImageProcessed
              then ps + jpeg_size
              else ps
      ss = sStandaloneSize stats
      ss' = case status of
              ImageStandalone -> ss + jpeg_size
              _               -> ss
      cs = sSidecarSize stats
      cs' = cs + maybe 0 fileSize (imgSidecarPath img)
      exif = imgExif img
      ymdate = imageYearMonth img
      camera = fromMaybe unknown (exifCamera exif)
      xsize = case imgRawPath img of
               Just f -> fileSize f
               Nothing -> case imgJpegPath img of
                            x:_ -> fileSize x
                            _   -> 0
      doubleUp x = (x, x)
      shutterCount = doubleUp <$> exifShutterCount exif
      -- TODO: do we want local time here, or to push forward the zone
      -- as well?
      captureDate = doubleUp <$> exifLocalCreateDate exif
      ocBuilder i = ocFromSize xsize i ymdate captureDate
      lens = exifLens exif
      lensOcc = ocBuilder lens
      cameraOcc = ocBuilder (CameraInfo camera shutterCount)
      ms = foldl' (\s f -> s + fileSize f) 0 (maybeToList (imgMasterMov img) ++ imgMovs img)
      ms' = sMovieSize orig + ms
      us = sum . map fileSize . imgUntracked $ img
      us' = sUntrackedSize orig + us
      people = sPeople stats `Set.union` exifPeople exif
  in stats { sRawSize = rs'
           , sProcSize = ps'
           , sStandaloneSize = ss'
           , sSidecarSize = cs'
           , sMovieSize = ms'
           , sUntrackedSize = us'
           , sByCamera = Map.insertWith (<>) camera cameraOcc (sByCamera orig)
           , sByLens = Map.insertWith (<>) (liName lens) lensOcc(sByLens orig)
           , sPeople = people
           , sDateRange = mergeMinMaxPair (sDateRange orig) captureDate
           }

computeImagesStats :: (Foldable a) => a Image -> Stats
computeImagesStats = Data.Foldable.foldl' updateStatsWithPic zeroStats

computeFolderStats :: PicDir -> Stats
computeFolderStats =
  computeImagesStats . pdImages

data StrictPair a b = StrictPair !a !b

computeRepoStats :: RepoDirs -> RepoStats
computeRepoStats =
  (\(StrictPair a b) -> RepoStats a b) .
  Map.foldl' (\(StrictPair picstats fcstats) dir ->
                let stats = pdStats dir
                    fc = folderClassFromStats stats
                    picstats' = sumStats picstats stats
                    fcstats' = Map.insertWith (+) fc 1 fcstats
                in StrictPair picstats' fcstats'
             ) (StrictPair zeroStats Map.empty)

repoGlobalExif :: RepoDirs -> GroupExif
repoGlobalExif =
  Map.foldl' (\e f -> e <> pdExif f) def

updateRepo :: Ctx -> Repository -> IO Bool
updateRepo ctx new = atomically $ do
  let rc = ctxRepo ctx
  -- TODO: replace this with stateTVar when LTS 13.
  current <- readTVar rc
  let do_update = repoSerial current <= repoSerial new
  -- TODO: add logging for prevented overwrites.
  when do_update $ do
    writeTVar rc $! new
    flushSearchCache (ctxSearchCache ctx)
  return do_update

tryUpdateRepo :: Ctx -> Repository -> IO Repository
tryUpdateRepo ctx new = do
  owning <- updateRepo ctx new
  unless owning $ throwString "Repository ownership changed, aborting"
  return new

-- FIXME: move to STM?
getSearchResults :: Ctx -> SearchResults -> UrlParams -> IO SearchResults
getSearchResults ctx lazy key = atomically $ do
  let searchCache = ctxSearchCache ctx
  oldCache <- readTVar searchCache
  let (val, newCache) =
        fromMaybe (force lazy `seq` (lazy, LRU.insert key lazy oldCache))
          (LRU.lookup key oldCache)
  writeTVar searchCache $! newCache
  return val

flushSearchCache :: TVar SearchCache -> STM ()
flushSearchCache = flip writeTVar emptySearchCache

-- | Selects the best master file between two masters.
--
-- We select in the order of extensions as defined in the
-- config. Normally both files should have a raw extension, but we
-- should also handle the \"impossible\" cases: no matching extensions
-- for either file. If the extensions are the same, this will still
-- chew through the list. In both these cases, we (randomly) choose
-- the first file.
isBetterMaster :: [FilePath] -> FilePath -> FilePath -> Bool
isBetterMaster []     _ _ = True
isBetterMaster (e:_ ) a _ | e == a     = True
isBetterMaster (e:_ ) _ b | e == b     = False
isBetterMaster (_:es) a b = isBetterMaster es a b

-- | Selects the best master file when merging images.
--
-- This simply chooses the first hard master file, and returns updated
-- soft master flag and potentially a demoted (usually soft) master to
-- jpeg. It can also be there are two of what we consider real master
-- files, in which case use the config order to select between
-- these. Usually one is really a derivation of the other.
selectMasterFile :: [FilePath] -> (Image -> Maybe File)
                 -> Image -> Image
                 -> (Maybe File, Bool, [File])
selectMasterFile rexts pathfn x y =
  let xsoft = flagsSoftMaster $ imgFlags x
      ysoft = flagsSoftMaster $ imgFlags y
      xraw = pathfn x
      yraw = pathfn y
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
                                let ext_extract = drop 1 . takeExtension . Text.unpack . fileName
                                    x_ext = ext_extract xf
                                    y_ext = ext_extract yf
                                in if isBetterMaster rexts x_ext y_ext
                                     then (xraw, False, [yf])
                                     else (yraw, False, [xf])
       (Just _, Nothing)   -> (xraw, xsoft, [])
       (Nothing, Just _)   -> (yraw, ysoft, [])
       _                   -> (Nothing, False, [])

mergePictures :: Config -> Image -> Image -> Image
mergePictures c x y =
  let (rawPath, softmaster, extrajpeg) = selectMasterFile (cfgRawExts c) imgRawPath x y
      -- FIXME: softmaster flag is not really per image… but seems rather per file.
      (masterMov, _, extramovs) = selectMasterFile (cfgRawExts c) imgMasterMov x y
      -- FIXME: we lose here sidecar files.
      sidecarPath = imgSidecarPath x `mplus` imgSidecarPath y
      jpegPath = imgJpegPath x ++ imgJpegPath y ++ extrajpeg
      movs = imgMovs x ++ imgMovs y ++ extramovs
      ityp = imgType x `max` imgType y
      untrk = imgUntracked x ++ imgUntracked y
      status = mkImageStatus c rawPath jpegPath sidecarPath (ityp /= MediaImage)
      exif = promoteFileExif
                (fileExif <$> rawPath) (fileExif <$> sidecarPath)
                (map fileExif jpegPath)
                (fileExif <$> masterMov) (map fileExif movs)
  in force $ x { imgRawPath     = rawPath
               , imgSidecarPath = sidecarPath
               , imgJpegPath    = jpegPath
               , imgFlags       = (imgFlags x) { flagsSoftMaster = softmaster }
               , imgMasterMov   = masterMov
               , imgMovs        = movs
               , imgUntracked   = untrk
               , imgType        = ityp
               , imgStatus      = status
               , imgExif        = exif
               }

mergeFolders :: Config -> PicDir -> PicDir -> PicDir
mergeFolders c x y =
  force $
  x { pdMainPath = pdMainPath bestMainPath
    , pdSecPaths = pdSecPaths x ++ pdMainPath otherMainPath:pdSecPaths y
    , pdImages = newimages
    , pdTimeSort = buildTimeSort newimages
    , pdYear = min <$> pdYear x <*> pdYear y <|>
               pdYear x <|>
               pdYear y
    , pdExif = buildGroupExif newimages
    -- Note: here we can't sum the stats from x and y, since their
    -- merge can change an image's type, thus throwing off the stats
    -- completely (e.g. in x is unprocessed, in y is standalone, in
    -- x+y is processed).
    , pdStats = computeImagesStats newimages
    }
  where
    (bestMainPath, otherMainPath) =
      case (compare `on` (\z -> (numRawPics z, numPics z))) x y of
                                     GT -> (x, y)
                                     _  -> (y, x)
    newimages = Map.unionWith (mergePictures c) (pdImages x) (pdImages y)

-- | Compute the number of pictures with an associated raw file.
numRawPics :: PicDir -> Int
numRawPics (pdStats -> s) =
  sRaw s + sProcessed s

numPics :: PicDir -> Int
numPics = Map.size . pdImages

numUnprocessedPics :: PicDir -> Int
numUnprocessedPics = sRaw . pdStats

numProcessedPics :: PicDir -> Int
numProcessedPics = sProcessed . pdStats

numStandalonePics :: PicDir -> Int
numStandalonePics = sStandalone . pdStats

hasViewablePics :: PicDir -> Bool
hasViewablePics folder =
  numProcessedPics folder > 0 ||
  numStandalonePics folder > 0

folderClass :: PicDir -> FolderClass
folderClass = folderClassFromStats . computeFolderStats

folderClassFromStats :: Stats -> FolderClass
folderClassFromStats stats@Stats {..} =
  let npics = sRaw + sStandalone + sProcessed + sOrphaned
      has_pics       = npics /= 0
      has_unproc     = sRaw /= 0
      has_standalone = sStandalone /= 0
      all_unproc     = sRaw == npics
      has_raw        = sRaw /= 0 || sProcessed /= 0
      has_orphaned   = sOrphaned /= 0
      has_movies     = sMovies /= 0
      conditions = (has_pics, all_unproc, has_unproc,
                    has_standalone, has_raw, has_orphaned,
                    has_movies)
  in case conditions of
       -- pics all_u  has_u  has_s  has_r  has_or has_m
       -- folder with no pics is empty
       (False, True , False, False, False, False, False) -> FolderEmpty
       -- folder with just movies is Processed, otherwise we don't care about movies.
       (False, True,  False, False, False, False, True)  -> FolderProcessed
       -- folder with all unprocessed is raw
       (True,  True , True , False, _    , False, _)     -> FolderRaw
       -- folder with some unprocessed (and maybe other types) is unprocessed
       (True,  False, True , _    , _    , _    , _)     -> FolderUnprocessed
       -- folder with no raw files is standalone
       (True,  False, False, True , False, False, _)     -> FolderStandalone
       -- folder with both standalone and some raw is mixed
       (True,  False, False, True , True , False, _)     -> FolderMixed
       -- folder with orphaned pictures is mixed
       (True,  False, _    , _    , _    , True,  _ )    -> FolderMixed
       -- othewise, folder is perfect - has only processed files
       (True,  False, False, False, True , False, _)     -> FolderProcessed
       _ -> error $ "Wrong computation in folderClass: stats=" ++ show stats
                    ++ ", conditions=" ++ show conditions

getDirContents' :: Bool -> Config -> FilePath -> IO ([FilePath], [InodeInfo])
getDirContents' filtered config root = do
  contents <- getDirectoryContents root
  let blkdirs = if filtered
                then blacklistedDirs config
                else neverRecurseDirs
      allowed_names = filter (`notElem` blkdirs) contents
  paths <-
    foldM (\acc path -> do
             stat <- getSymbolicLinkStatus $ root </> path
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

getDirContents :: Config -> FilePath -> IO ([FilePath], [InodeInfo])
getDirContents = getDirContents' True

-- | Scans one of the directories defined in the configuration.
scanBaseDir :: Ctx
            -> String
            -> Bool
            -> IO [PicDir]
scanBaseDir ctx root isSource = do
  (dirs, _) <- getDirContents (ctxConfig ctx) root
  concat <$> mapConcurrently (\p -> scanSubDir ctx (root </> p) isSource) dirs

-- | Scans a directory one level below a base dir. The actual
-- subdirectory name is currently discarded and will not appear in the
-- final folder names.
scanSubDir :: Ctx
           -> String
           -> Bool
           -> IO [PicDir]
scanSubDir ctx path isSource = do
  let config = ctxConfig ctx
  (dirpaths, _) <- getDirContents config path
  let allpaths' = filter (isOKDir config) dirpaths
  mapM (\s -> loadFolder ctx s (path </> s) isSource) allpaths'

-- | Recursively count number of items (images) under a directory.
countDir :: Config -> Int -> String -> IO Int
countDir config level path = do
  (dirpaths, iinfo) <- getDirContents config path
  -- TODO: This level thinig is ugly; the two-level hierarchy is rigid
  -- and thus needs hardcoding here and there. Urgh!
  let allpaths = if level == 2
                 then filter (isOKDir config) dirpaths
                 else dirpaths
  subdirs <- mapConcurrently (\p -> countDir config (level+1) (path </> p)) allpaths
  let total = sum subdirs + if level > 2 then length iinfo else 0
  return $!! total

-- | Recursively count number of all files and directories under a directory.
countDirRaw :: Config -> String -> IO Int
countDirRaw config path = do
  (dirpaths, iinfo) <- getDirContents' False config path
  subdirs <- mapConcurrently (\p -> countDirRaw config (path </> p)) dirpaths
  let total = sum subdirs + length iinfo
  return $!! total

-- | Run action on all files under a given directory.
allPathFiles :: Config -> FilePath -> (FilePath -> IO ()) -> FilePath -> IO ()
allPathFiles config root handler (makeRel -> subdir) = do
  let buildsub p = if subdir == "." then p else subdir </> p
  (dirpaths, iinfo) <- getDirContents' False config (root </> subdir)
  mapM_ (\ii -> handler (subdir </> inodeName ii)) iinfo
  mapM_ (allPathFiles config root handler . buildsub) dirpaths

addDirToRepo :: Config -> PicDir -> RepoDirs -> RepoDirs
addDirToRepo config dir =
  Map.insertWith (mergeFolders config) (pdName dir) dir

-- | Builds the filepath and filestatus pairs recursively for all
-- entries beneats a directory.
recursiveScanPath :: Config
                  -> FilePath
                  -> (FilePath -> FilePath)
                  -> IO [InodeInfo]
recursiveScanPath config root prepender = do
  (!dirs, !files) <- getDirContents config root
  let with_prefix = map (\ii -> ii { inodeName = prepender (inodeName ii) }) files
  subdirs <- mapM (\p -> recursiveScanPath config (root </> p)
                           (prepender . (p </>))) dirs
  return $ with_prefix ++ concat subdirs

-- | Strict application of the 'Just' constructor. This is useful as
-- the Maybe type is not strict in its contained value.
strictJust :: a -> Maybe a
strictJust !a = Just a

addImgs :: Config -> Map ImageName Image -> [Image] -> Map ImageName Image
addImgs config = foldl' (addImg config)

addImg :: Config -> Map ImageName Image -> Image -> Map ImageName Image
addImg config m i = Map.insertWith (mergePictures config) (imgName i) i m

buildGroupExif :: Map ImageName Image -> GroupExif
buildGroupExif =
  Map.foldl' (\e img -> addExifToGroup e (imgExif img)) def

buildTimeSort :: Map ImageName Image -> Set ImageTimeKey
buildTimeSort = Set.fromList . map imageTimeKey . Map.elems

-- | Builds a `PicDir` (folder) from an entire filesystem subtree.
loadFolder :: Ctx -> String -> FilePath -> Bool -> IO PicDir
loadFolder ctx name path isSource = do
  -- throwString "boo"
  let config = ctxConfig ctx
      scanProgress = ctxScanProgress ctx
  contents <- recursiveScanPath config path id
  (readexifs, lcache) <- getExif config path $ map inodeName contents
  let rawe = cfgRawExtsSet config
      side = cfgSidecarExts config
      jpeg = cfgJpegExts config
      move = cfgMovieExts config
      tname = Text.pack name
      ewarn txt = def { exifWarning = Set.singleton txt }
      dirpath = Text.pack path
      buildName = ImageName . Text.pack
      loadImage ii  =
        let file_name = inodeName ii
            file_text = Text.pack file_name
            (base_full, ext') = splitExtension file_name
            ext = Text.pack $ case ext' of
              '.':v -> v
              _     -> ext'
            base_name = dropCopySuffix config base_full
            exif = case file_text `Map.lookup` lcache of
                     Nothing         -> ewarn "Internal error: exif not read"
                     Just (Left msg) -> ewarn $ "Cannot read exif: " `Text.append` msg
                     Just (Right e)  -> e
            file_obj =
              File { fileName  = file_text
                   , fileCTime = inodeCTime ii
                   , fileMTime = inodeMTime ii
                   , fileSize  = inodeSize ii
                   , fileDir   = dirpath
                   , fileExif  = exif
                   }
            just_file = strictJust file_obj
            isSoftMaster = is_jpeg && isSource
            raw_file =
              if ext `Set.member` rawe || isSoftMaster
              then just_file
              else Nothing
            sidecar_file =
              if ext `Set.member` side
              then just_file
              else Nothing
            is_jpeg = ext `Set.member` jpeg
            is_mov = ext `Set.member` move
            m_mov = if is_mov && isSource
                      then just_file
                      else Nothing
            jpeg_file = [file_obj | is_jpeg && not isSource]
            p_mov = [file_obj | is_mov && not isSource]
            snames = expandRangeFile config base_name
            range = case fromNullable snames of
                      Nothing -> Nothing
                      Just s  -> Just (buildName $ head s, buildName $ last s)
            flags = Flags {
              flagsSoftMaster = isSoftMaster
              }
            simgs = map (\expname ->
                           mkImage config (buildName expname) tname
                                   Nothing Nothing [file_obj] Nothing [] [] Nothing MediaImage emptyFlags
                        ) snames
            onlySidecar = isNothing raw_file && null jpeg_file && isJust sidecar_file
            mtype = if | is_mov     -> MediaMovie
                       | null untrk -> MediaImage
                       | otherwise  -> MediaUnknown
            untrk = case (raw_file, jpeg_file, sidecar_file, m_mov, p_mov) of
              (Nothing, [], Nothing, Nothing, []) -> [file_obj]
              _                                   -> []
            img = force $
              mkImage config (buildName base_name) tname
              raw_file sidecar_file jpeg_file m_mov p_mov
              untrk range mtype flags
        in (img, if onlySidecar then [] else simgs)
      (images, shadows) =
        foldl' (\(images', shadows') f ->
                  let (img, newss) = loadImage f
                  in (addImg config images' img, addImgs config shadows' newss)
               ) (Map.empty, Map.empty) contents
  let year = Map.foldl' (\a img ->
                           (min <$> a <*> imageYear img) <|>
                           a <|>
                           imageYear img
                        ) Nothing images
      exif = buildGroupExif images
      timesort = buildTimeSort images
      totalitems = length contents
      noopexifs = max (totalitems - readexifs) 0
      pstats = computeImagesStats images
  -- FIXME: incProgress is always called with an empty error list?
  atomically $ modifyTVar' scanProgress (incProgress [] noopexifs readexifs)
  return $!! PicDir tname dirpath [] images timesort shadows year exif pstats

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
           (imgSidecarPath img) (imgJpegPath img)
           (imgMasterMov img) (imgMovs img) (imgUntracked img)
           (imgRange img) (imgType img) (imgFlags img)

resolveProcessedRanges :: Config -> PicDir -> PicDir
resolveProcessedRanges config picd =
  let images' =
        Map.foldl (\accimgs img ->
                     case maybeUpdateStandaloneRange  config picd img of
                       Nothing   -> accimgs
                       Just img' -> Map.insert (imgName img') img' accimgs)
             (pdImages picd) (pdImages picd)
  in picd { pdImages = images' }

-- | Checks if a given cache file is obsolete.
examineCacheFile :: FilePath       -- ^ Absolute prefix path on the filesystem
                 -> TVar Progress  -- ^ For tracking progress
                 -> Set TextL.Text -- ^ Set of real image files (absolute paths)
                 -> LogFn          -- ^ Logger function
                 -> FilePath       -- ^ Relative path to the item being examined
                 -> IO ()
examineCacheFile prefix cleanProgress iset logger path = do
  let fpath = TextL.pack (pathSeparator:path)
      found = maybe False (`isPrefixOf` fpath) $ Set.lookupLE fpath iset
  modifier <- if found
              then return incNoop
              else cleanCacheFile prefix logger path
  atomically $ modifyTVar cleanProgress modifier

-- | Cleans up a given cache file.
cleanCacheFile :: FilePath       -- ^ Absolute prefix path on the filesystem
               -> LogFn          -- ^ Logger function
               -> FilePath       -- ^ Relative path to the item being examined
               -> IO (Progress -> Progress)
cleanCacheFile prefix logger path = do
  res <- deleteCacheFile prefix path
  case res of
    Just err -> do
      logger LevelError $ "Failed to delete path: " <> toLogStr err
      return $ incErrors (Text.pack path) err
    Nothing  -> do
      logger LevelDebug $ "Cleaned obsolete path '" <> toLogStr path <> "'"
      return incDone

-- | Cleanups given directories under the cache dir.
cleanupCache :: Ctx         -- ^ Context
             -> Repository  -- ^ Repository with current set of images
             -> [FilePath]  -- ^ Set of directories to cleanup
             -> Int         -- ^ Target item count goal
             -> IO Progress -- ^ Result of cleanup
cleanupCache ctx repo alldirs cachecount = do
  let cleanProgress = ctxCleanProgress ctx
  atomically $ writeTVar cleanProgress (def { pgGoal = cachecount })
  let imgs = allRepoFiles repo
      iset = Set.fromList $ map filePath imgs
      config = ctxConfig ctx
      cacheDir = cfgCacheDir config
      handler = examineCacheFile cacheDir cleanProgress iset (ctxLogger ctx)
  mapM_ (allPathFiles config cacheDir handler) alldirs
  readTVarIO cleanProgress

-- | Resets the repository to empty with an increase serial number.
--
-- The logic here needs to be kept in sync with updateRepo.
newRepo :: TVar Repository -> STM Repository
newRepo rc = do
  old <- readTVar rc
  let new = old { repoSerial = repoSerial old + 1
                , repoStatus = RepoStarting
                }
  -- Note: it is safe to update the repository without flushing the
  -- cache, as the image date has not been changed, only the serial
  -- and status; hence, not going through updateRepo.
  writeTVar rc $! new
  return new

waitForScan :: Ctx -> IO Repository
waitForScan ctx = atomically $ do
  scanner <- readTVar (ctxScanner ctx)
  maybe (readTVar (ctxRepo ctx)) waitSTM scanner

launchScanFileSystem :: Ctx -> IO ()
launchScanFileSystem ctx =
  bracketOnError
    (atomically $ newRepo rc)
    (\new -> tryUpdateRepo ctx (new { repoStatus = RepoError "unknown"}))
    $ \newrepo -> do
      newscanner <- async (scanFSWrapper ctx newrepo)
      currentSC <- atomically $ swapTVar scannerThread (Just newscanner)
      case currentSC of
        Nothing -> return ()
        Just t -> do
          logfn LevelInfo "Cancelling previous scanner"
          cancel t
          logfn LevelInfo "Cancel done"
      return ()
     where rc = ctxRepo ctx
           logfn = ctxLogger ctx
           scannerThread = ctxScanner ctx

scanFSWrapper :: Ctx -> Repository -> IO Repository
scanFSWrapper ctx newrepo = do
  scanner <- async $ scanFilesystem ctx newrepo
  result <- waitCatch scanner
  case result of
    Left err ->
      tryUpdateRepo ctx newrepo {repoStatus = RepoError (sformat shown err)}
    Right r -> return r

scanFilesystem :: Ctx -> Repository -> IO Repository
scanFilesystem ctx newrepo = do
  let logfn = ctxLogger ctx
      config = ctxConfig ctx
      scanProgress = ctxScanProgress ctx
  logfn LevelInfo "Launching scan filesystem"
  r1 <- tryUpdateRepo ctx (newrepo { repoStatus = RepoStarting })
  let srcdirs = map (, True)  (cfgSourceDirs config)
      outdirs = map (, False) (cfgOutputDirs config)
      alldirs = map fst $ srcdirs ++ outdirs
      alldirsAsRelative = map makeRel alldirs
  logfn LevelInfo $ "Counting images under " <> toLogStr (show alldirs)
  itemcounts <- mapConcurrently (countDir config 1) alldirs
  let cachePaths = map (cfgCacheDir config </>) alldirsAsRelative
  logfn LevelInfo $ "Counting cache items under " ++ toLogStr (show cachePaths)
  -- Ensure cache top dirs are created if missing (likely only during
  -- bootstrap, but better to always redo during scanning).
  mapConcurrently_ (createDirectoryIfMissing True) cachePaths
  cachecounts' <- mapConcurrently (countDirRaw config) cachePaths
  let cachecounts = sum cachecounts'
  atomically $ writeTVar scanProgress (def { pgGoal = sum itemcounts })
  start <- getZonedTime
  let ws = WorkStart { wsStart = start }
  r2 <- tryUpdateRepo ctx (r1 { repoStatus = RepoScanning { rsScanGoal = ws } })
  asyncDirs <- mapConcurrently (uncurry (scanBaseDir ctx))
                 $ srcdirs ++ outdirs
  logfn LevelInfo "Finished scanning directories"
  scanned <- readTVarIO scanProgress
  end <- getZonedTime
  let repo = foldl' (flip (addDirToRepo config)) Map.empty $ concat asyncDirs
  let repo' = Map.map (resolveProcessedRanges config .
                       mergeShadows config) repo
      stats = computeRepoStats repo'
      rexif = repoGlobalExif repo'
      -- For render stats:
      allsizes  = cfgAutoImageSizes config
      -- urgh, renderable images needs a full repo…
      allimgs = renderableImages (newrepo { repoDirs = repo' })
      totalrender = length allsizes * length allimgs
      wrscan = WorkResults { wrStart = start
                           , wrEnd = end
                           , wrDone = scanned
                           }
      wsrender= WorkStart { wsStart = end
                          }
  repo_as <- tryUpdateRepo ctx r2 { repoDirs   = repo'
                                  , repoStats  = stats
                                  , repoExif   = rexif
                                  , repoStatus = RepoRendering { rsScanResults = wrscan,
                                                                 rsRenderGoal = wsrender }
                                  }
  writeDiskCache config repo_as
  logfn LevelInfo "Finished building repo, starting rendering"
  rendered <- forceBuildThumbCaches config (ctxRenderProgress ctx) repo_as totalrender
  endr <- getZonedTime
  let wrrender = WorkResults { wrStart = end
                             , wrEnd = endr
                             , wrDone = rendered
                             }
      wrstatus = RepoCleaning { rsScanResults = wrscan
                              , rsRenderResults = wrrender
                              , rsCleanGoal = WorkStart { wsStart = endr }
                              }
  repo_ar' <- evaluate $ force $ repo_as { repoStatus = wrstatus }
  repo_ar <- tryUpdateRepo ctx repo_ar'
  logfn LevelInfo "Finished rendering, starting cleanup"
  clean_pg <- cleanupCache ctx repo_ar alldirsAsRelative cachecounts
  endc <- getZonedTime
  let wrclean = WorkResults { wrStart = endr
                            , wrEnd = endc
                            , wrDone = clean_pg
                            }
      status = RepoFinished { rsScanResults = wrscan
                            , rsRenderResults = wrrender
                            , rsCleanResults = wrclean
                            }
  repo_ac <- evaluate $ force $ repo_ar { repoStatus = status }
  r4 <- tryUpdateRepo ctx repo_ac
  writeDiskCache config r4
  logfn LevelInfo "Finished cleaning up, all done"
  return r4

-- | Computes the list of images that can be rendered.
renderableImages :: Repository -> [Image]
renderableImages = filterImagesByClass [ImageUnprocessed, ImageProcessed, ImageStandalone]

forceBuildThumbCaches :: Config -> TVar Progress -> Repository -> Int -> IO Progress
forceBuildThumbCaches config renderProgress repo totalrender = do
  atomically $ writeTVar renderProgress (def { pgGoal = totalrender})
  let images = renderableImages repo
      imageForError i = sformat (stext % "/" % stext % " at resolution " % int)
                        (imgParent i) (unImageName (imgName i))
      thbuild i = mapM_ (\size -> do
                            res <- imageAtRes config i . Just . ImageSize $ size
                            let modifier = case res of
                                  Left err            -> incErrors (imageForError i size) (Text.pack $ show err)
                                  Right (False, _, _) -> incNoop
                                  Right (True, _, _)  -> incDone
                            atomically $ modifyTVar renderProgress modifier
                        )
                    (cfgAutoImageSizes config)
  mapM_ thbuild images
  readTVarIO renderProgress

repoDiskFile :: String
repoDiskFile = "repo"

repoDiskPath :: Config -> FilePath -> FilePath
repoDiskPath config path = cachedBasename config path ("cache" ++ devSuffix)

writeDiskCache :: Config -> Repository -> IO ()
writeDiskCache config repo =
  writeCacheFile config repoDiskFile repoDiskPath (Data.Store.encode (config, repo))

readDiskCache :: Config -> IO (Maybe Repository)
readDiskCache config = do
  contents <- readCacheFile config repoDiskFile repoDiskPath False []
  let decoded = contents >>= either (const Nothing) Just . Data.Store.decode
  return $ case decoded of
    Nothing -> Nothing
    Just (c, repo) -> if c == config
                      then Just repo
                      else Nothing

loadCacheOrScan :: Ctx
                -> Repository
                -> IO Repository
loadCacheOrScan ctx old@(repoStatus -> RepoEmpty) = do
  cachedRepo <- readDiskCache (ctxConfig ctx)
  let rescanning = rescanningRepository old
      logfn = ctxLogger ctx
  case cachedRepo of
     Nothing    -> do
       logfn LevelInfo "No cache data or data incompatible, scanning filesystem"
       launchScanFileSystem ctx
       return rescanning
     Just cached_r@(repoStatus -> RepoFinished {}) -> do
       logfn LevelInfo "Cached data available, skipping scan"
       -- Note: this shouldn't fail, since an empty repo happens only
       -- upon initial load, and (initial) empty repos have a serial
       -- number of zero while any valid cache will have a positive
       -- serial.
       tryUpdateRepo ctx cached_r
     Just unfinished -> do
       logfn LevelWarn . toLogStr $ "Unfinished cache found, state: " ++ show (repoStatus unfinished)
       logfn LevelInfo "Restarting scan"
       launchScanFileSystem ctx
       return rescanning
loadCacheOrScan _ orig = return orig

-- | Returns the current repository.
--
-- Nowadays this always returns the repository, which might be empty.
getRepo :: Ctx -> IO Repository
getRepo = readTVarIO . ctxRepo

-- | Returns the progress of the repository scanner.
--
-- If the scan has finished, this is the value of all files and
-- directories scanned (mildly interesting). If not, then this value
-- is the running state.
getProgress :: Ctx -> IO Progress
getProgress = readTVarIO . ctxScanProgress

-- | Returns the progress of the render thread.
getRenderProgress :: Ctx -> IO Progress
getRenderProgress = readTVarIO . ctxRenderProgress

getCleanProgress :: Ctx -> IO Progress
getCleanProgress = readTVarIO . ctxCleanProgress

scanAll :: Ctx -> IO Repository
scanAll ctx = do
  current <- getRepo ctx
  loadCacheOrScan ctx current

filterImagesByClass :: [ImageStatus] -> Repository -> [Image]
filterImagesByClass classes =
  filterImagesBy (\p -> imgStatus p `elem` classes)

filterImagesBy :: (Image -> Bool) -> Repository -> [Image]
filterImagesBy flt =
  foldl' (\pics folder ->
            Map.foldl' (\l img -> if flt img
                                  then img:l
                                  else l) pics (pdImages folder)
         ) [] . Map.elems . repoDirs

filterFoldersBy :: (PicDir -> Bool) -> Repository -> RepoDirs
filterFoldersBy flt = Map.filter flt . repoDirs

imageHasImages :: Image -> Bool
imageHasImages Image{..} =
  isJust imgRawPath || isJust imgSidecarPath || not (null imgJpegPath)

imageHasMovies :: Image -> Bool
imageHasMovies img =
  isJust (imgMasterMov img) || (not . null . imgMovs) img

imageHasUntracked :: Image -> Bool
imageHasUntracked =
  not . null . imgUntracked

allImageFiles :: Image -> [File]
allImageFiles Image{..} =
  catMaybes [imgRawPath, imgSidecarPath, imgMasterMov] ++
  imgJpegPath ++ imgMovs ++ imgUntracked

-- | Returns the list of (likely, but not guaranteed) viewable files
-- for an image.
allViewableImageFiles :: Image -> [File]
allViewableImageFiles Image{..} =
  imgJpegPath ++ catMaybes [imgRawPath, imgMasterMov] ++ imgMovs

addDirFiles :: PicDir -> [File] -> [File]
addDirFiles dir =
  flip (Map.foldl' (\fs img -> allImageFiles img ++ fs)) (pdImages dir)

allRepoFiles :: Repository -> [File]
allRepoFiles =
  Map.foldl' (flip addDirFiles) [] . repoDirs

-- | Computes optimal image preview size.
--
-- Assuming the passed list is sorted, the function computes the
-- smallest size that is larger or equal than the required image
-- size. If none is found, it means we're requesting a size larger
-- than any of our allowed-to-be-cached sizes, so return Nothing.
findBestSize :: ImageSize -> Set Int -> Maybe Int
findBestSize (ImageSize i) = Set.lookupLE i

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
loadCachedOrBuild :: Config -> FilePath -> FilePath -> MimeType
                  -> POSIXTime -> ImageSize
                  -> IO (Bool, MimeType, FilePath)
loadCachedOrBuild config origPath bytesPath mime mtime size = do
  let res = findBestSize size (cfgAllImageSizes config)
  case res of
    Nothing -> return (False, mime, bytesPath)
    Just res' -> do
      let geom = show res' ++ "x" ++ show res'
          fpath = scaledImagePath config origPath res'
          isThumb = res' <= cfgThumbnailSize config
          fmt = if isThumb then "png" else "jpg"
      -- Note: this calls getFileStatus not getSymbolicLinkStatus
      -- since this is a file, not a directory, so symlinks to
      -- somewhere else are OK-ish; we do not recurse into the target
      -- directory.
      stat <- tryJust (guard . isDoesNotExistError) $ getFileStatus fpath
      let needsGen = case stat of
                       Left _   -> True
                       Right st -> modificationTimeHiRes st < mtime
      when needsGen $ do
        let operators = if isThumb
                          then ["-thumbnail", geom, "-background", "none", "-gravity", "center", "-extent", geom]
                          else ["-resize", geom]
            (parent, _) = splitFileName fpath
            outFile = fmt ++ ":" ++ fpath
        createDirectoryIfMissing True parent
        -- FIXME: this is a stopgap fix, make nicer error handling (not all) and log errors too.
        (exitCode, out, err) <- readProcess (proc "convert" (concat [[bytesPath], operators, [outFile]])) `catch`
                                (\e -> let e' = sformat shown (e :: SomeException)
                                       in throwIO . ImageError $ e')
        when (exitCode /= ExitSuccess) . throwIO . ImageError . TextL.toStrict . Text.decodeUtf8 $ err `BSL.append` out
      return (needsGen, sformat ("image/" % string) fmt, fpath)

-- | Ordered list of tags that represent embedded images.
previewTags :: [String]
previewTags = ["JpgFromRaw", "PreviewImage"]

-- | List of extensions that are directly viewable.
--
-- TODO: move to config.
asIsViewableExtensions :: [Text]
asIsViewableExtensions =
  map ('.' `Text.cons`)
  ["png", "jpg", "jpeg", "tiff", "tif"]

-- | Minimum thumbnail size to consider it a valid image.
minImageSize :: Int64
minImageSize = 512

-- | Ensure parent path for path exists.
ensureParent :: FilePath -> IO ()
ensureParent path = do
  let (dir, _) = splitFileName path
  createDirectoryIfMissing True dir

-- | Extracts and saves an embedded thumbnail from an image.
--
-- The extract image type is presumed (and required) to be jpeg.
extractEmbedded :: Config -> LazyText -> String -> IO (Either Text (MimeType, FilePath))
extractEmbedded config path tag = do
  let pathS = TextL.unpack path
      outputPath = embeddedImagePath config pathS
      args = [
        "-b",
        "-" ++ tag,
        pathS
        ]
      -- FIXME: embedded images are assumed JPEGs.
      mime = "image/jpeg"
      goodRet = Right (mime, outputPath)
  gotThumbnail <- fileExist outputPath
  if not gotThumbnail
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
          ensureParent outputPath
          BSL.writeFile outputPath out
          return goodRet
        else
          return $ Left $ TextL.toStrict $ Text.decodeUtf8 err -- partial function!
    else
      -- TODO: mtime-based regen.
      return goodRet

-- | Extract the first valid thumbnail from an image.
bestEmbedded :: Config -> LazyText -> IO (Either Text (MimeType, FilePath))
bestEmbedded config path =
  go config path previewTags
  where go _ _ [] = return $ Left "No tags available"
        go c p (x:xs) = do
          r <- extractEmbedded c p x
          case r of
            Left _ -> case xs of
              [] -> return r
              _  -> go c p xs
            Right _ -> return r

-- | Checks is a file is directly viewable.
viewableAsIs :: Text -> Bool
viewableAsIs path =
  any (`Text.isSuffixOf` path) asIsViewableExtensions

-- | Extracts and saves the first frame from a movie.
extractFirstFrame :: Config -> LazyText -> IO (Either Text (MimeType, FilePath))
extractFirstFrame config path = do
  let pathS = TextL.unpack path
      outputPath = embeddedImagePath config pathS
      args = [
        "-nostdin",
        "-y",
        "-i",
        pathS,
        "-r", "1",
        "-frames", "1",
        "-f", "image2",
        outputPath
        ]
      -- FIXME: Extracted frames ('image2') are assumed jpeg.
      mime = "image/jpeg"
      goodRet = Right (mime, outputPath)
  gotFirstFrame <- fileExist outputPath
  if not gotFirstFrame
    then do
      ensureParent outputPath
      let pconfig =
            setStdin closed
            . setCloseFds True
            $ proc "ffmpeg" args
      (exitCode, out, err) <- readProcess pconfig
      if exitCode == ExitSuccess
        then
          return goodRet
        else
          let err' = Text.decodeUtf8 err -- note partial function!
              out' = Text.decodeUtf8 out
              msg = out' `TextL.append` err'
          in return . Left . TextL.toStrict $ msg
    else
      return goodRet

-- | Compute a presumed jpeg's mime type.
jpegMimeType :: File -> MimeType
jpegMimeType = fileMimeType "image/jpeg"

-- | Returns a viewable version of an image.
--
-- For a processed file, return it directly; for a raw file, extract
-- the embedded image, if any; etc.
getViewableVersion :: Config -> Image -> IO (File, FilePath, MimeType, POSIXTime)
getViewableVersion config img
  | j:_ <- imgJpegPath img =
      return (j, TextL.unpack (filePath j), jpegMimeType j, fileLastTouch j)
  | Just j <- imgRawPath img,
    True <- flagsSoftMaster (imgFlags img) =
      return (j, TextL.unpack (filePath j), jpegMimeType j, fileLastTouch j)
  | Just r <- imgRawPath img = do
      -- TODO: there should be some tests to check which version is returned.
      viewable <- if viewableAsIs (fileName r)
                  then return $ Right (jpegMimeType r, TextL.unpack $ filePath r)
                  else bestEmbedded config (filePath r)
      case viewable of
        Left msg -> throwIO $ ImageError msg
        Right (mime, path) -> do
          mtime <- filePathLastTouch path
          return (r, path, mime, mtime)
  | m:_ <- imgMovs img ++ maybeToList (imgMasterMov img) = do
      embedded <- bestEmbedded config (filePath m)
      case embedded of
        Left _ -> do
          firstFrame <- extractFirstFrame config (filePath m)
          case firstFrame of
            Left msg' -> throwIO $ ImageError msg'
            Right (mime, path) -> do
              mtime <- filePathLastTouch path
              return (m, path, mime, mtime)
        Right (mime, path) -> do
          mtime <- filePathLastTouch path
          return (m, path, mime, mtime)
  | otherwise = throwIO ImageNotViewable

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
imageAtRes :: Config -> Image -> Maybe ImageSize -> IO (Either ImageError (Bool, MimeType, FilePath))
imageAtRes config img size = try $ do
  (origFile, path, mime, mtime) <- getViewableVersion config img
  case size of
    Nothing -> return (False, mime, path)
    Just s  -> loadCachedOrBuild config (TextL.unpack $ filePath origFile) path mime mtime s

imgProblems :: Image -> Set Text
imgProblems =
  Set.map ("exif: " <>) . exifWarning . imgExif

pdProblems :: PicDir -> NameStats Text
pdProblems =
  foldl' (\m probs ->
            if null probs
              then Map.insertWith (+) Nothing 1 m
              else foldl' (\m' e -> Map.insertWith (+) (Just e) 1 m') m probs)
  Map.empty . map (Set.toList . imgProblems) . Map.elems . pdImages

repoProblems :: Repository -> NameStats Text
repoProblems =
  Map.unionsWith (+) . map pdProblems . Map.elems . repoDirs

transformForFile :: File -> Transform
transformForFile  = affineTransform . exifOrientation . fileExif

transformForImage :: Image -> Transform
transformForImage =
  maybe def transformForFile . fileToView

fileToView :: Image -> Maybe File
fileToView img =
  case imgJpegPath img of
    f:_ -> Just f
    []  -> imgRawPath img
