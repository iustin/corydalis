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
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
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
            , Exif(..)
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
            , scanAll
            , forceScanAll
            , isProcessed
            , isUnprocessed
            , isStandalone
            , folderClass
            , folderClassFromStats
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
            , filterImagesBy
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
            , allRepoFiles
            , imageHasImages
            , imageHasMovies
            , imageHasUntracked
            , imageYear
            , imageDate
            , imageTimeKey
            , SearchResults
            , getSearchResults
            , imgProblems
            , pdProblems
            , repoProblems
            , transformForFile
            , transformForImage
            -- Test export :/
            , mkImage
            ) where

import           Control.Applicative
import           Control.Concurrent.Async
import           Control.Concurrent.STM.TVar
import           Control.DeepSeq
import           Control.Monad
import           Control.Monad.STM
import qualified Data.ByteString.Lazy        as BSL (append, length, writeFile)
import           Data.Default                (Default, def)
import           Data.Function               (on)
import           Data.Int                    (Int64)
import           Data.List
import           Data.LruCache               (LruCache)
import qualified Data.LruCache               as LRU
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as Map
import           Data.Maybe
import           Data.Semigroup
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Data.Store
import           Data.Store.TH
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Lazy              as TextL
import qualified Data.Text.Lazy.Encoding     as Text (decodeUtf8)
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           Prelude
import           System.Directory
import           System.Exit
import           System.FilePath
import           System.IO.Error
import           System.IO.Unsafe
import           System.Log.FastLogger       (toLogStr)
import           System.Posix.Files          hiding (fileSize)
import qualified System.Posix.Files          (fileSize)
import           System.Posix.Types
import           System.Process.Typed
import qualified Text.Regex.TDFA             as TDFA
import           UnliftIO.Exception

import           Cache
import           Compat.Orphans              ()
import           Exif
import           Settings.Development
import           Types

blacklistedDirs :: Config -> [String]
blacklistedDirs config = [".", ".."] ++ cfgBlacklistedDirs config

isOKDir :: Config -> String -> Bool
isOKDir cfg = TDFA.match (reRegex $ cfgDirRegex cfg)

dropCopySuffix :: Config -> String -> String
dropCopySuffix cfg name =
  case TDFA.match (reRegex $ cfgCopyRegex cfg) name of
    [_:base:_] -> base
    _          -> name

maybeRead :: (Read a) => String -> Maybe a
maybeRead s = case reads s of
                [(i, [])] -> Just i
                _         -> Nothing

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
    { imgName        :: !Text
    , imgParent      :: !Text
    , imgRawPath     :: !(Maybe File)
    , imgSidecarPath :: !(Maybe File)
    , imgJpegPath    :: ![File]
    , imgMasterMov   :: !(Maybe File)
    , imgMovs        :: ![File]
    , imgUntracked   :: ![File]
    , imgRange       :: !(Maybe (Text, Text))
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
  date <- exifCreateDate exif
  let day = localDay date
      (y, _, _) = toGregorian day
  return y

-- | The year of the image, as determined from Exif data.
imageYearMonth :: Image -> Maybe (Int, Int)
imageYearMonth img = do
  let exif = imgExif img
  date <- exifCreateDate exif
  let day = localDay date
      (y, m, _) = toGregorian day
  -- Let's hope the year/month stay sane here.
  return (fromIntegral y, fromIntegral m)

imageDate :: Image -> Maybe LocalTime
imageDate = exifCreateDate . imgExif

type ImageTimeKey = (Maybe LocalTime, Text)

imageTimeKey :: Image -> ImageTimeKey
imageTimeKey img = (imageDate img, imgName img)

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
mkImageStatus _ (Just _) []    _         _     = ImageRaw
mkImageStatus _ Nothing  (_:_) _         _     = ImageStandalone
mkImageStatus _ (Just _) (_:_) _         _     = ImageProcessed

mkImage :: Config
        -> Text               -- ^ Name
        -> Text               -- ^ Parent
        -> Maybe File         -- ^ Raw path
        -> Maybe File         -- ^ Sidecar path
        -> [File]             -- ^ Jpegs
        -> Maybe File         -- ^ Master movie
        -> [File]             -- ^ Movies
        -> [File]             -- ^ Untracked files
        -> Maybe (Text, Text) -- ^ Range
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
  , pdImages   :: !(Map Text Image)
  , pdTimeSort :: !(Set ImageTimeKey)
  , pdShadows  :: !(Map Text Image)
  , pdYear     :: !(Maybe Integer)  -- ^ The approximate year of the
                                    -- earliest picture.
  , pdExif     :: !GroupExif
  } deriving (Show)

instance NFData PicDir where
  rnf PicDir{..} = rnf pdName     `seq`
                   rnf pdMainPath `seq`
                   rnf pdSecPaths `seq`
                   rnf pdImages   `seq`
                   rnf pdShadows  `seq`
                   rnf pdYear      `seq`
                   rnf pdExif

type RepoDirs = Map Text PicDir

-- | Status of a repository.
data RepoStatus = RepoEmpty    -- ^ Only to be used at application
                               -- startup time!
                | RepoStarting -- ^ Denotes an empty, in-process of
                               -- being rescanned repo.
                | RepoScanning !WorkStart
                | RepoRendering !WorkResults !WorkStart
                | RepoFinished !WorkResults !WorkResults
                | RepoError !Text
  deriving (Show)

instance NFData RepoStatus where
  rnf RepoEmpty              = ()
  rnf RepoStarting           = ()
  rnf (RepoScanning ws)      = rnf ws
  rnf (RepoRendering wr ws)  = rnf wr `seq` rnf ws
  rnf (RepoFinished wr1 wr2) = rnf wr1 `seq` rnf wr2
  rnf (RepoError t)          = rnf t

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

-- | Type alias for the trends keys.
type TrendsKey = (Int, Int)

-- | Type alias for per-month statistics.
type Trends = Map TrendsKey Integer

data Occurrence a = Occurrence
  { ocFiles    :: !Integer
  , ocFileSize :: !FileOffset
  , ocFolders  :: !Integer
  , ocData     :: !a
  , ocTrends   :: !Trends
  } deriving (Show)

instance Default a => Default (Occurrence a) where
  def = Occurrence 0 0 0 def Map.empty

instance (Semigroup a, Default a) => Monoid (Occurrence a) where
  mempty = def

instance Semigroup a => Semigroup (Occurrence a) where
  x <> y = Occurrence { ocFiles = ocFiles x + ocFiles y
                      , ocFileSize = ocFileSize x + ocFileSize y
                      , ocFolders = ocFolders x + ocFolders y
                      , ocData = ocData x <> ocData y
                      , ocTrends = Map.unionWith (+) (ocTrends x) (ocTrends y)
                      }

instance NFData a => NFData (Occurrence a) where
  rnf occ = rwhnf occ `seq` rnf (ocData occ)

ocFromSize :: FileOffset -> a -> Maybe TrendsKey -> Occurrence a
ocFromSize size d tk =
  Occurrence { ocFiles = 1
             , ocFileSize = size
             , ocFolders = 0
             , ocData = d
             , ocTrends = maybe Map.empty (`Map.singleton` 1) tk
             }

data CameraInfo = CameraInfo
  { ciName         :: !Text
  , ciShutterCount :: !(Maybe (Integer, Integer))
  , ciDateRange    :: !(Maybe (LocalTime, LocalTime))
  } deriving (Eq, Show, Ord)

instance NFData CameraInfo where
  rnf CameraInfo{..} = rnf ciName `seq`
                       rnf ciShutterCount `seq`
                       rnf ciDateRange

-- | Min-max merge of two pairs.
minMaxPairMerge :: (Ord a) => (a, a) -> (a, a) -> (a, a)
minMaxPairMerge (xmin, xmax) (ymin, ymax) =
  (xmin `min` ymin, xmax `max` ymax)

-- | Merge two Maybe values using a custom function.
mergeMM :: Maybe a -> Maybe a -> (a -> a -> a) -> Maybe a
mergeMM Nothing x _          = x
mergeMM x Nothing _          = x
mergeMM (Just x) (Just y) fn = Just $ fn x y

instance Semigroup CameraInfo where
  x <> y = x { ciShutterCount = mergeMM (ciShutterCount x) (ciShutterCount y) minMaxPairMerge
             , ciDateRange = mergeMM (ciDateRange x) (ciDateRange y) minMaxPairMerge
             }

instance Default CameraInfo where
  def = CameraInfo unknown Nothing Nothing

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

-- | The empty (zero) stats.
zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 Map.empty Map.empty

instance Default Stats where
  def = zeroStats

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
sumStats (Stats r1 s1 p1 h1 u1 m1 rs1 ps1 ss1 sd1 us1 ms1 sc1 sl1)
         (Stats r2 s2 p2 h2 u2 m2 rs2 ps2 ss2 sd2 us2 ms2 sc2 sl2 ) =
  Stats (r1 + r2) (s1 + s2) (p1 + p2) (h1 + h2) (u1 + u2) (m1 + m2)
        (rs1 + rs2) (ps1 + ps2) (ss1 + ss2) (sd1 + sd2) (us1 + us2) (ms1 + ms2)
        sc sl
  where sc = Map.unionWith mappend sc1 sc2
        sl = Map.unionWith (<>) sl1 sl2

updateStatsWithPic :: Stats -> Image -> Stats
updateStatsWithPic orig img =
  let status = imgStatus img
      stats = case imgType img of
        MediaMovie   -> orig { sMovies    = sMovies orig    + 1 }
        MediaUnknown -> orig { sUntracked = sUntracked orig + 1 }
        MediaImage   -> case status of
          ImageRaw        -> orig { sRaw        = sRaw orig        + 1 }
          ImageStandalone -> orig { sStandalone = sStandalone orig + 1 }
          ImageProcessed  -> orig { sProcessed  = sProcessed orig  + 1 }
          ImageOrphaned   -> orig { sOrphaned   = sOrphaned orig   + 1 }
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
      lens = exifLens exif
      lensOcc = ocFromSize xsize lens ymdate
      doubleUp x = (x, x)
      shutterCount = doubleUp <$> exifShutterCount exif
      captureDate = doubleUp <$> exifCreateDate exif
      cameraOcc = ocFromSize xsize (CameraInfo camera shutterCount captureDate) ymdate
      ms = foldl' (\s f -> s + fileSize f) 0 (maybeToList (imgMasterMov img) ++ imgMovs img)
      ms' = sMovieSize orig + ms
      us = sum . map fileSize . imgUntracked $ img
      us' = sUntrackedSize orig + us
  in stats { sRawSize = rs'
           , sProcSize = ps'
           , sStandaloneSize = ss'
           , sSidecarSize = cs'
           , sMovieSize = ms'
           , sUntrackedSize = us'
           , sByCamera = Map.insertWith (<>) camera cameraOcc (sByCamera orig)
           , sByLens = Map.insertWith (<>) (liName lens) lensOcc(sByLens orig)
           }

computeFolderStats :: PicDir -> Stats
computeFolderStats =
  Map.foldl' updateStatsWithPic zeroStats . pdImages

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

-- | Type alias for search results, weakly capture-time-sorted.
type SearchResults = Map (Text, ImageTimeKey) Image

-- | Type of the search cache.
type SearchCache = LruCache UrlParams SearchResults

{-# NOINLINE repoCache #-}
repoCache :: TVar Repository
repoCache = unsafePerformIO $ newTVarIO startupRepository

updateRepo :: TVar Repository -> Repository -> IO Bool
updateRepo rc new = do
  -- TODO: replace this with stateTVar when LTS 13.
  atomically $ do
    current <- readTVar rc
    let update = repoSerial current <= repoSerial new
    when update $ do
      writeTVar rc $! new
      flushSearchCache
    return update

tryUpdateRepo :: TVar Repository -> Repository -> IO ()
tryUpdateRepo rc new = do
  owning <- updateRepo rc new
  unless owning $ throwString "Repository ownership changed, aborting"

{-# NOINLINE scannerThread #-}
scannerThread :: TVar (Maybe (Async ()))
scannerThread = unsafePerformIO $ newTVarIO Nothing

{-# NOINLINE scanProgress #-}
scanProgress :: TVar Progress
scanProgress = unsafePerformIO $ newTVarIO def

{-# NOINLINE renderProgress #-}
renderProgress :: TVar Progress
renderProgress = unsafePerformIO $ newTVarIO def

-- TODO: hardcoded cache size. Hmm...
emptySearchCache :: SearchCache
emptySearchCache = LRU.empty 10

{-# NOINLINE searchCache #-}
searchCache :: TVar SearchCache
searchCache = unsafePerformIO $ newTVarIO emptySearchCache

getSearchResults :: SearchResults -> UrlParams -> IO SearchResults
getSearchResults lazy key = atomically $ do
  oldCache <- readTVar searchCache
  let (val, newCache) = case (LRU.lookup key oldCache) of
        Nothing -> force lazy `seq` (lazy, LRU.insert key lazy oldCache)
        Just v  -> v
  writeTVar searchCache $! newCache
  return val

flushSearchCache :: STM ()
flushSearchCache = writeTVar searchCache emptySearchCache

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
    }
  where
    (bestMainPath, otherMainPath) =
      case (compare `on` (\z -> (numRawPics z, numPics z))) x y of
                                     GT -> (x, y)
                                     _  -> (y, x)
    newimages = Map.unionWith (mergePictures c) (pdImages x) (pdImages y)

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
            -> String
            -> Bool
            -> IO [PicDir]
scanBaseDir config base isSource = do
  (dirs, _) <- getDirContents config base
  concat <$> mapConcurrently (\p -> scanSubDir config (base </> p) isSource) dirs

-- | Scans a directory one level below a base dir. The actual
-- subdirectory name is currently discarded and will not appear in the
-- final folder names.
scanSubDir :: Config
           -> String
           -> Bool
           -> IO [PicDir]
scanSubDir config path isSource = do
  (dirpaths, _) <- getDirContents config path
  let allpaths' = filter (isOKDir config) dirpaths
  mapM (\s -> loadFolder config s (path </> s) isSource) allpaths'

-- | Recursively count number of items under a directory.
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

addDirToRepo :: Config -> PicDir -> RepoDirs -> RepoDirs
addDirToRepo config dir =
  Map.insertWith (mergeFolders config) (pdName dir) dir

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

addImgs :: Config -> Map Text Image -> [Image] -> Map Text Image
addImgs config = foldl' (addImg config)

addImg :: Config -> Map Text Image -> Image -> Map Text Image
addImg config m i = Map.insertWith (mergePictures config) (imgName i) i m

buildGroupExif :: Map Text Image -> GroupExif
buildGroupExif =
  Map.foldl' (\e img -> addExifToGroup e (imgExif img)) def

buildTimeSort :: Map Text Image -> Set ImageTimeKey
buildTimeSort = Set.fromList . map imageTimeKey . Map.elems

-- | Builds a `PicDir` (folder) from an entire filesystem subtree.
loadFolder :: Config -> String -> FilePath -> Bool -> IO PicDir
loadFolder config name path isSource = do
  -- throwString "boo"
  contents <- recursiveScanPath config path id
  (readexifs, lcache) <- getExif config path $ map inodeName contents
  let rawe = cfgRawExtsSet config
      side = cfgSidecarExts config
      jpeg = cfgJpegExts config
      move = cfgMovieExts config
      tname = Text.pack name
      ewarn txt = def { exifWarning = Set.singleton txt }
      dirpath = Text.pack path
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
            range = case snames of
                      [] -> Nothing
                      _  -> Just (Text.pack $ head snames, Text.pack $ last snames)
            flags = Flags {
              flagsSoftMaster = isSoftMaster
              }
            simgs = map (\expname ->
                           mkImage config (Text.pack expname) tname
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
              mkImage config (Text.pack base_name) tname
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
  atomically $ modifyTVar' scanProgress (incProgress 0 noopexifs readexifs)
  return $!! PicDir tname dirpath [] images timesort shadows year exif

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

-- | Resets the repository to empty with an increase serial number.
--
-- The logic here needs to be kept in sync with updateRepo.
newRepo :: TVar Repository -> STM Repository
newRepo rc = do
  old <- readTVar rc
  let new = old { repoSerial = repoSerial old + 1
                , repoStatus = RepoStarting
                }
  writeTVar rc $! new
  return new

launchScanFileSystem :: Config -> TVar Repository -> LogFn -> IO ()
launchScanFileSystem config rc logfn =
  bracketOnError
    (atomically $ newRepo rc)
    (\new -> tryUpdateRepo rc (new { repoStatus = RepoError "unknown"}))
    $ \newrepo -> do
      newscanner <- async (scanFSWrapper config rc newrepo logfn)
      currentSC <- atomically $ swapTVar scannerThread (Just newscanner)
      case currentSC of
        Nothing -> return ()
        Just t -> do
          logfn "Cancelling previous scanner"
          cancel t
          logfn "Cancel done"
      return ()

scanFSWrapper :: Config -> TVar Repository -> Repository -> LogFn -> IO ()
scanFSWrapper config rc newrepo logfn = do
  scanner <- async $ scanFilesystem config rc newrepo logfn
  result <- waitCatch scanner
  case result of
    Left err -> tryUpdateRepo rc newrepo { repoStatus = RepoError (Text.pack $ show err) }
    Right _ -> return ()

scanFilesystem :: Config -> TVar Repository -> Repository -> LogFn -> IO ()
scanFilesystem config rc newrepo logfn = do
  logfn "Launching scan filesystem"
  tryUpdateRepo rc (newrepo { repoStatus = RepoStarting })
  let srcdirs = zip (cfgSourceDirs config) (repeat True)
      outdirs = zip (cfgOutputDirs config) (repeat False)
  itemcounts <- mapConcurrently (countDir config 1) $ map fst $ srcdirs ++ outdirs
  atomically $ writeTVar scanProgress def
  start <- getZonedTime
  let ws = WorkStart { wsStart = start, wsGoal = sum itemcounts }
  tryUpdateRepo rc (newrepo { repoStatus = RepoScanning ws })
  asyncDirs <- mapConcurrently (uncurry (scanBaseDir config))
                 $ srcdirs ++ outdirs
  logfn "Finished scanning directories"
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
                           , wrGoal = wsGoal ws
                           , wrDone = scanned
                           }
      wsrender= WorkStart { wsStart = end
                          , wsGoal = totalrender
                          }
      repo'' = newrepo { repoDirs   = repo'
                       , repoStats  = stats
                       , repoExif   = rexif
                       , repoStatus = RepoRendering wrscan wsrender
                       }
  tryUpdateRepo rc repo''
  writeDiskCache config repo''
  logfn "Finished building repo, starting rendering"
  forceBuildThumbCaches config repo''
  endr <- getZonedTime
  rendered <- readTVarIO renderProgress
  let wrrender = WorkResults { wrStart = end
                             , wrEnd = endr
                             , wrGoal = totalrender
                             , wrDone = rendered
                             }
  let repo''' = repo'' { repoStatus = RepoFinished wrscan wrrender }
  tryUpdateRepo rc repo'''
  writeDiskCache config repo'''
  logfn "Finished rendering, all done"
  return ()

-- | Computes the list of images that can be rendered.
renderableImages :: Repository -> [Image]
renderableImages = filterImagesByClass [ImageRaw, ImageProcessed, ImageStandalone]

forceBuildThumbCaches :: Config -> Repository -> IO ()
forceBuildThumbCaches config repo = do
  atomically $ writeTVar renderProgress def
  -- throwString "boo"
  let images = renderableImages repo
      builder i = mapM_ (\size -> do
                            res <- imageAtRes config i . Just . ImageSize $ size
                            let modifier = case res of
                                  Left _              -> incErrors
                                  Right (False, _, _) -> incNoop
                                  Right (True, _, _)  -> incDone
                            atomically $ modifyTVar renderProgress modifier
                        )
                    (cfgAutoImageSizes config)
  mapM_ builder images

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

loadCacheOrScan :: Config
                -> Repository
                -> LogFn
                -> IO Repository
loadCacheOrScan config old@(repoStatus -> RepoEmpty) logfn = do
  cachedRepo <- readDiskCache config
  case cachedRepo of
     Nothing    -> do
       logfn "No cache data or data incompatible, scanning filesystem"
       launchScanFileSystem config repoCache logfn
       return $ rescanningRepository old
     Just cache@(repoStatus -> RepoFinished {}) -> do
       logfn "Cached data available, skipping scan"
       -- Note: this shouldn't fail, since an empty repo happens only
       -- upon initial load, and (initial) empty repos have a serial
       -- number of zero while any valid cache will have a positive
       -- serial.
       tryUpdateRepo repoCache cache
       return cache
     Just unfinished -> do
       logfn . toLogStr $ "Unfinished cache found, state: " ++ show unfinished
       logfn "Restarting scan"
       launchScanFileSystem config repoCache logfn
       return $ rescanningRepository old
loadCacheOrScan _ orig _ = return orig

-- | Returns the current repository.
--
-- Nowadays this always returns the repository, which might be empty.
getRepo :: IO Repository
getRepo = readTVarIO repoCache

-- | Returns the progress of the repository scanner.
--
-- If the scan has finished, this is the value of all files and
-- directories scanned (mildly interesting). If not, then this value
-- is the running state.
getProgress :: IO Progress
getProgress = readTVarIO scanProgress

-- | Returns the progress of the render thread.
getRenderProgress :: IO Progress
getRenderProgress = readTVarIO renderProgress

scanAll :: Config -> LogFn -> IO Repository
scanAll config logfn = do
  current <- getRepo
  loadCacheOrScan config current logfn

forceScanAll :: Config -> LogFn -> IO ()
forceScanAll config =
  launchScanFileSystem config repoCache

computeStandaloneDirs :: Repository -> [PicDir]
computeStandaloneDirs =
  filter hasStandalonePics . Map.elems . repoDirs

filterDirsByClass :: [FolderClass] -> Repository -> [PicDir]
filterDirsByClass classes =
  filter ((`elem` classes) . folderClass) .
  Map.elems . repoDirs

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
          format = if isThumb then "png" else "jpg"
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
            outFile = format ++ ":" ++ fpath
        createDirectoryIfMissing True parent
        (exitCode, out, err) <- readProcess $ proc "convert" (concat [[bytesPath], operators, [outFile]])
        when (exitCode /= ExitSuccess) . throwIO . ImageError . TextL.toStrict . Text.decodeUtf8 $ err `BSL.append` out
      return (needsGen, Text.pack $ "image/" ++ format, fpath)

-- | Ordered list of tags that represent embedded images.
previewTags :: [String]
previewTags = ["JpgFromRaw", "PreviewImage"]

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

-- | Extracts and saves the first frame from a movie.
extractFirstFrame :: Config -> LazyText -> IO (Either Text (MimeType, FilePath))
extractFirstFrame config path = do
  let pathS = TextL.unpack path
      outputPath = embeddedImagePath config pathS
      args = [
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
  exists <- fileExist outputPath
  if not exists
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
      embedded <- bestEmbedded config (filePath r)
      case embedded of
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

pdProblems :: PicDir -> NameStats
pdProblems =
  foldl' (\m probs ->
            if null probs
              then Map.insertWith (+) Nothing 1 m
              else foldl' (\m' e -> Map.insertWith (+) (Just e) 1 m') m probs)
  Map.empty . map (Set.toList . imgProblems) . Map.elems . pdImages

repoProblems :: Repository -> NameStats
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

$(makeStore ''CameraInfo)
$(makeStore ''Occurrence)
$(makeStore ''File)
$(makeStore ''MediaType)
$(makeStore ''Flags)
$(makeStore ''Image)
$(makeStore ''Stats)
$(makeStore ''RepoStats)
$(makeStore ''PicDir)
$(makeStore ''RepoStatus)
$(makeStore ''Repository)
