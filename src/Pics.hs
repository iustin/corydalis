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
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

module Pics ( PicDir(..)
            , Image(..)
            , ImageTimeKey
            , ImageError(..)
            , MediaType(..)
            , MimeType
            , File(..)
            , fileFullPath
            , fileRelPath
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
            , countImagesBy
            , imageDateRange
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
            , bestMovie
            , ensureStorageFile
            , movieToServe
            , checkMagickPolicy
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
            , ImageNeighbours(..)
            , imageNeighbours
            , SearchCache
            , imgProblems
            , pdProblems
            , repoProblems
            , transformForFile
            , transformForImage
            , updateStatsWithPic
            , computeImagesStats
#ifdef TEST
            , convertProcess
            , jpegMagic
            , pngMagic
            , PreviewFormat(..)
            , previewArgs
            , checkPreviewImage
            , checkPreviewFile
            , buildPreviewFile
            , tryImageError
            , checkEmbeddedImage
            , extractEmbedded
            , embeddedArgs
            , extractFirstFrame
            , firstFrameArgs
            , mkImage
            , buildTimeSort
            , buildGroupExif
            , addDirToRepo
            , InodeInfo(..)
            , inodeFullName
            , mkFileFromInode
            , implicitEventFromDateRange
            , startupRepository
            , writeDiskCache
            , loadImage
            , buildFolderFromInodes
            , loadFolder
            , folderYamlFile
            , getDirContents
            , recursiveScanPath
            , scanBaseDir
            , countDir
            , dropCopySuffix
            , expandRangeFile
            , maxRangeExpansion
            , isOKDir
            , makeRel
            , mkImageStatus
            , isBetterMaster
            , selectMasterFile
            , mergePictures
            , mergeFolders
            , mergeShadows
            , resolveProcessedRanges
            , maybeUpdateStandaloneRange
            , findBestSize
            , viewableAsIs
            , imageDecoder
            , movieDemuxer
            , embeddedImagePath
            , newRepo
            , forceBuildThumbCaches
            , runScanAction
            , scaledImagePath
            , cacheFileNeedsBuild
            , upToDateCacheFile
            , filePathLastTouch
            , writeViaTempFile
            , cacheFileEndings
            , isLiveCacheFile
            , scaledCacheNeedsBuild
            , imagePendingSizes
            , imageViewSource
            , ViewKind(..)
            , splitPathExt
            , isKnownMediaInode
            , renderableImages
#endif
            ) where

import           Control.Applicative
import           Control.DeepSeq
import qualified Data.ByteString         as BS (hGet)
import qualified Data.ByteString.Lazy    as BSL (append, isPrefixOf, length, null,
                                                pack)
import qualified Data.Foldable
import           Data.LruCache           (LruCache)
import qualified Data.LruCache           as LRU
import qualified Data.Map.Strict         as Map
import           Data.Semigroup
import qualified Data.Set                as Set
import           Data.Store              (Store)
import qualified Data.Store
import           Data.Char               (isAlphaNum, isAscii)
import qualified Data.Text               as Text
import qualified Data.Text.Short         as TS
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           Debug.Trace.String
import           System.Directory        hiding (isSymbolicLink)
import qualified System.Environment
import qualified System.IO
import           System.Exit
import           System.FilePath
import           System.Log.FastLogger   (toLogStr)
import qualified System.Posix.Files      (fileSize)
import           System.Posix.Files      hiding (fileSize)
import           System.Posix.Types
import           System.Posix.IO         (OpenFileFlags (..), OpenMode (..),
                                          closeFd, defaultFileFlags,
                                          fdToHandle, openFd)
import           System.Process.Typed
import qualified Text.Regex.TDFA         as TDFA
import           UnliftIO.Exception


import           AtomTypes               (EventKindOp, extractEventType)
import           Cache
import           Compat.Orphans          ()
import           Exif
import           Import.NoFoundation     hiding (fileName, fileSize)
import           Utils.Tools
import           Utils.Storage           (isCanonicalStoragePath)
import           Stats                   (CameraInfo (..), DateRange,
                                          Occurrence (..), Trends,
                                          mergeMinMaxPair, ocFromSize)
import           Utils.Parsing           (parseDecimal)
import           Utils.Yaml              (loadYaml, maxYamlSize)

type Ctx = Context Repository SearchCache

-- TODO: move this to IO, so that callers don't have to know about
-- atomically? [cleanup]
initContext :: Config -> LogFn
            -> ToolLimits  -- ^ The limits of the tools building previews.
            -> ToolLimits  -- ^ The limits of the scan's exiftool runs.
            -> STM Ctx
initContext config logfn renderTools scanTools =
  newContext config logfn renderTools scanTools startupRepository emptySearchCache

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

-- | Upper bound on names generated from a range file.
--
-- Filenames like @img_3982-1773958749527@ match the range regex but
-- are timestamps, not brackets; expanding them OOMs.
maxRangeExpansion :: Int
maxRangeExpansion = 1024

expandRangeFile :: Config -> Text -> [Text]
expandRangeFile cfg name =
  case TDFA.match (reRegex $ cfgRangeRegex cfg) name of
    [[_, root, begin, end]] -> let ib = parseDecimal begin::Either Text Int
                                   ie = parseDecimal end
                                   formatPadded = sformat (left (length begin) '0' %. int)
                               in case (ib, ie) of
                                    (Right b, Right e)
                                      | let n = toInteger e - toInteger b + 1
                                      , n >= 1
                                      , n <= toInteger maxRangeExpansion ->
                                          [root `Text.append` formatPadded i | i <- [b..e]]
                                    _ -> []
    _ -> []

-- Readability alias.
type MimeType = Text

data File = File
  { fileName   :: !ShortText
  , fileDirs   :: !SymbolizedItem
  , fileCTime  :: !POSIXTime
  , fileMTime  :: !POSIXTime
  , fileSize   :: !FileOffset
  , fileParent :: !SymbolizedItem
  , fileExif   :: !Exif
  } deriving (Show, Eq, Generic)

instance Store File

instance NFData File where
  rnf File{..} =
    rnf fileName   `seq`
    rnf fileDirs   `seq`
    rnf fileCTime  `seq`
    rnf fileMTime  `seq`
    fileSize       `seq` -- plain type, weak form is enough
    rnf fileParent `seq`
    rnf fileExif

-- | The full path for a file.
fileFullPath :: File -> FilePath
fileFullPath File{..} =
  System.FilePath.joinPath [deSymbolizeItem' fileParent, deSymbolizeItem' fileDirs, TS.toString fileName]

-- | The relative path for a file, under its parent.
fileRelPath :: File -> FilePath
fileRelPath File{..} =
  System.FilePath.joinPath [deSymbolizeItem' fileDirs, TS.toString fileName]

-- | Try to find a valid mime type for a file.
--
-- The type found by exiftool is derived from the file's contents, which
-- can be anything, e.g. an SVG or HTML document named @photo.jpg@; served
-- with its real type, such a document would run scripts in the context of
-- the site. Hence the type is only used if it is an image or video type
-- that browsers do not treat as a document, and the default otherwise.
fileMimeType :: Text -> File -> Text
fileMimeType d f =
  case deSymbolizeItem <$> exifMimeType (fileExif f) of
    Just t | isSafeMediaType t -> t
    _                          -> d

-- | Whether a MIME type is a plain image or video type.
--
-- SVG is excluded since it can contain scripts, and so is anything that is
-- not a simple type token, which also rules out header injection.
isSafeMediaType :: Text -> Bool
isSafeMediaType t =
  Text.all validChar t &&
  (("image/" `Text.isPrefixOf` lower && not ("image/svg" `Text.isPrefixOf` lower)) ||
   "video/" `Text.isPrefixOf` lower)
  where lower = Text.toLower t
        validChar c = isAscii c && (isAlphaNum c || c `elem` ("+-./" :: String))

-- | Flags (on an image or a directory) showing exceptional
-- statuses.
{-# ANN Flags ("HLint: ignore Use newtype instead of data"::String) #-}
data Flags = Flags
  { flagsSoftMaster :: !Bool
  } deriving (Show, Eq, Generic)

instance Store Flags

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
               deriving (Show, Eq, Ord, Generic)

instance Store MediaType

instance NFData MediaType where
  rnf _ = ()

data Image = Image
    { imgName        :: !ImageName
    , imgParent      :: !ShortText
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
    } deriving (Show, Eq, Generic)

instance Store Image

instance NFData Image where
  rnf Image{..} = rnf imgName        `seq`
                  rnf imgParent      `seq`
                  rnf imgRawPath     `seq`
                  rnf imgSidecarPath `seq`
                  rnf imgJpegPath    `seq`
                  rnf imgMasterMov   `seq`
                  rnf imgMovs        `seq`
                  rnf imgUntracked   `seq`
                  rnf imgRange       `seq`
                  rnf imgExif        `seq`
                  rnf imgType        `seq`
                  rnf imgStatus      `seq`
                  rnf imgFlags

data InodeInfo = InodeInfo
  { inodeName  :: !FilePath    -- | The basename of the file.
  , inodeDirs  :: ![FilePath]  -- | The directories leading to the file, in reverse order.
                               --   Not all path elements are represented,
                               --   only ones under which we recurse.
  , inodeIsDir :: !Bool
  , inodeMTime :: !POSIXTime
  , inodeCTime :: !POSIXTime
  , inodeSize  :: !FileOffset
  }

-- | Return the full path for an inode, including the sub-directories.
inodeFullName  :: InodeInfo -> FilePath
inodeFullName InodeInfo{..} =
  System.FilePath.joinPath . reverse $ inodeName : inodeDirs

instance NFData InodeInfo where
  rnf InodeInfo{..} = rnf inodeName   `seq`
                      rnf inodeDirs   `seq`
                      rnf inodeIsDir  `seq`
                      inodeMTime      `seq`
                      inodeCTime      `seq`
                      inodeSize       `seq`
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

-- | Extracts the year from a timestamp
localDateToYear :: LocalTime -> Integer
localDateToYear date =
    let day = localDay date
        (y, _, _) = toGregorian day
    in y

-- | The year of the image, as determined from Exif data.
imageYear :: Image -> Maybe Integer
imageYear img = do
  let exif = imgExif img
  date <- exifLocalCreateDate exif
  return $ localDateToYear date

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
        -> ShortText          -- ^ Parent
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
  { pdName      :: !ShortText
  , pdMainPath  :: !ShortText
  , pdSecPaths  :: ![ShortText]
  , pdImages    :: !(Map ImageName Image)
  , pdTimeSort  :: !(Set ImageTimeKey)
  , pdShadows   :: !(Map ImageName Image)
  , pdUntracked :: ![File]           -- ^ Files of unknown type, not treated as images.
  , pdYear      :: !(Maybe Integer)  -- ^ The approximate year of the
                                     -- earliest picture.
  , pdTimestamp :: !(Maybe LocalTime)  -- ^ The approximate date of the
                                       -- first picture.
  , pdExif      :: !GroupExif
  , pdStats     :: !Stats
  , pdEvent     :: !(Maybe Event)
  } deriving (Show, Generic)

instance Store PicDir

instance NFData PicDir where
  rnf PicDir{..} = rnf pdName      `seq`
                   rnf pdMainPath  `seq`
                   rnf pdSecPaths  `seq`
                   rnf pdImages    `seq`
                   rnf pdTimeSort  `seq`
                   rnf pdShadows   `seq`
                   rnf pdUntracked `seq`
                   rnf pdYear      `seq`
                   rnf pdTimestamp `seq`
                   rnf pdExif      `seq`
                   rnf pdStats     `seq`
                   rnf pdEvent

type RepoDirs = Map ShortText PicDir

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
  deriving (Show, Generic)

instance Store RepoStatus

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
  } deriving (Show, Generic)

instance Store Repository

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

-- | Counts of folders grouped by their event kind.
type EventKindStats = Map EventKindOp Int

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
  , sByCamera       :: !(Map SymbolizedItem (Occurrence CameraInfo))
  , sByLens         :: !(Map SymbolizedItem (Occurrence LensInfo))
  , sPeople         :: !(Set SymbolizedItem)
  , sDateRange      :: !(Maybe DateRange)
  } deriving (Show, Generic)

instance Store Stats

instance NFData Stats where
  rnf Stats{..} = rnf sByCamera  `seq`
                  rnf sByLens    `seq`
                  rnf sPeople    `seq`
                  rnf sDateRange

data RepoStats = RepoStats
  { rsPicStats   :: !Stats
  , rsFCStats    :: !FolderClassStats
  , rsEventStats :: !EventKindStats
  } deriving (Show, Generic)

instance Store RepoStats

instance NFData RepoStats where
  rnf RepoStats{..} = rnf rsPicStats `seq`
                      rnf rsFCStats `seq`
                      rnf rsEventStats

instance Default RepoStats where
  def = RepoStats def def def


-- | Type alias for image search results, weakly capture-time-sorted.
type SearchResultsPics = Map (ShortText, ImageTimeKey) Image

-- | Overall search results type alias.
type SearchResults = (SearchResultsPics, Map ShortText Image)

-- | Type of the search cache.
type SearchCache = LruCache UrlParams SearchResults

emptySearchCache :: SearchCache
emptySearchCache = LRU.empty 10

-- | The empty (zero) stats.
zeroStats :: Stats
zeroStats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 Map.empty Map.empty Set.empty Nothing

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
      camera = fromMaybe unknownSymbol $ exifCamera exif
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
           , sByLens = Map.insertWith (<>) (liName lens) lensOcc (sByLens orig)
           , sPeople = people
           , sDateRange = mergeMinMaxPair (sDateRange orig) captureDate
           }

computeImagesStats :: (Foldable a) => a Image -> Stats
computeImagesStats = Data.Foldable.foldl' updateStatsWithPic zeroStats

addUntrackedFiles :: [File] -> Stats -> Stats
addUntrackedFiles files stats =
  stats
    { sUntracked = sUntracked stats + length files
    , sUntrackedSize = sUntrackedSize stats + sum (map fileSize files)
    }

computeFolderStats :: PicDir -> Stats
computeFolderStats dir =
  addUntrackedFiles (pdUntracked dir) (computeImagesStats (pdImages dir))

data StrictTriple a b c = StrictTriple !a !b !c

computeRepoStats :: RepoDirs -> RepoStats
computeRepoStats =
  (\(StrictTriple a b c) -> RepoStats a b c) .
  Map.foldl' (\(StrictTriple picstats fcstats eventstats) dir ->
                let stats = pdStats dir
                    fc = folderClassFromStats stats
                    picstats' = sumStats picstats stats
                    fcstats' = Map.insertWith (+) fc 1 fcstats
                    eventstats' = Map.insertWith (+) (extractEventType $ pdEvent dir) 1 eventstats
                in StrictTriple picstats' fcstats' eventstats'
             ) (StrictTriple zeroStats Map.empty Map.empty)

repoGlobalExif :: RepoDirs -> GroupExif
repoGlobalExif =
  Map.foldl' (\e f -> e <> pdExif f) def

updateRepo :: Ctx -> Repository -> IO Bool
updateRepo ctx new = atomically $ do
  let rc = ctxRepo ctx
  -- Note: the below can't be replaced via stateTVar, since it does write
  -- to the search cache too, thus the transformation function is not
  -- pure.
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

checkRepoOwnership :: Ctx -> Repository -> IO ()
checkRepoOwnership ctx candidate = do
  current <- readTVarIO (ctxRepo ctx)
  unless (repoSerial current <= repoSerial candidate) $
    throwString "Repository ownership changed, aborting"

-- | Returns the search results for the given key, from the cache if
-- present, otherwise the given (lazy) results, cached.
--
-- The caller passes the results unevaluated (see 'searchImages'), so
-- that a cache hit doesn't compute them. On a miss, they are fully
-- evaluated before being cached, so that the cache only holds evaluated
-- results: an unevaluated entry would keep the repository it is
-- computed from alive, and leave the work to whoever reads it next.
--
-- This evaluation happens inside the transaction ('stateTVar' forces
-- the result pair), so a concurrent change of the cache makes the
-- transaction run again. That is cheap, though: the results are a
-- value built outside the transaction, so the work already done on them
-- is kept, and only the cache lookup and insertion are repeated. Two
-- identical searches at the same time both compute their results.
getSearchResults :: Ctx -> SearchResults -> UrlParams -> IO SearchResults
getSearchResults ctx lazy key =
  atomically $ stateTVar (ctxSearchCache ctx) $ \oldCache ->
    fromMaybe
      (force lazy `seq` (lazy, LRU.insert key lazy oldCache))
      (LRU.lookup key oldCache)

-- | The images around a given image in search results, as used for
-- navigating between them.
data ImageNeighbours = ImageNeighbours
  { nbFirst      :: Image         -- ^ The first image in the results.
  , nbPrevFolder :: Maybe Image   -- ^ The first image of the previous folder.
  , nbPrev       :: Maybe Image   -- ^ The previous image.
  , nbNext       :: Maybe Image   -- ^ The next image.
  , nbNextFolder :: Maybe Image   -- ^ The first image of the next folder.
  , nbLast       :: Image         -- ^ The last image in the results.
  } deriving (Show, Eq)

-- | Computes the neighbours of an image in the given search results.
--
-- Images are ordered by folder, then by their time key (date, then
-- name, so images without a date come first in their folder), and
-- folders by name. The image need not be part of the results: the
-- previous and next items are found by its position in that order. If
-- the results are empty, the first and last images are the image itself.
imageNeighbours :: SearchResults -> Image -> ImageNeighbours
imageNeighbours (images, folders) img =
  ImageNeighbours
    { nbFirst      = maybe img snd $ Map.lookupMin images
    , nbPrevFolder = snd <$> Map.lookupLT folder folders
    , nbPrev       = snd <$> Map.lookupLT key images
    , nbNext       = snd <$> Map.lookupGT key images
    , nbNextFolder = snd <$> Map.lookupGT folder folders
    , nbLast       = maybe img snd $ Map.lookupMax images
    }
  where folder = imgParent img
        key = (folder, imageTimeKey img)

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
                                -- TODO: move extension into the file structure.
                                let ext_extract = drop 1 . takeExtension . TS.toString . fileName
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
    , pdUntracked = newuntracked
    , pdYear = min <$> pdYear x <*> pdYear y <|>
               pdYear x <|>
               pdYear y
    , pdTimestamp = min <$> pdTimestamp x <*> pdTimestamp y <|>
                    pdTimestamp x <|> pdTimestamp y
    , pdExif = buildGroupExif newimages
    -- Note: here we can't sum the stats from x and y, since their
    -- merge can change an image's type, thus throwing off the stats
    -- completely (e.g. in x is unprocessed, in y is standalone, in
    -- x+y is processed).
    , pdStats = stats
    -- FIXME: record error if we have duplicate event information
    , pdEvent = pickEvent (pdEvent x) (pdEvent y)
                <|> implicitEventFromDateRange (pdName x) (sDateRange stats)
    }
  where
    (bestMainPath, otherMainPath) =
      case (compare `on` (\z -> (numRawPics z, numPics z))) x y of
                                     GT -> (x, y)
                                     _  -> (y, x)
    newimages = Map.unionWith (mergePictures c) (pdImages x) (pdImages y)
    newuntracked = pdUntracked x ++ pdUntracked y
    stats = addUntrackedFiles newuntracked (computeImagesStats newimages)
    pickEvent a b = explicitEvent a <|> explicitEvent b <|> a <|> b
    explicitEvent mev@(Just ev) =
      case eventSource ev of
        EventExplicit {} -> mev
        EventImplicit {} -> Nothing
    explicitEvent Nothing = Nothing

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

-- | The reason a directory entry is skipped, if it is neither a
-- directory nor a regular file.
--
-- Symbolic links are not followed, as they could point outside the
-- configured directories, and e.g. make any file readable by the
-- application available for download; other special files (pipes,
-- sockets, devices) are not pictures, and reading them could block.
skipReason :: FileStatus -> Maybe Text
skipReason stat
  | isSymbolicLink stat = Just "symbolic link, not followed"
  | isDirectory stat || isRegularFile stat = Nothing
  | otherwise = Just "not a regular file or directory, ignored"

-- | Lists a directory, returning the sub-directories, the regular
-- files, and the skipped entries (see 'skipReason') with the reason.
getDirContents' :: Bool -> Config -> FilePath
                -> IO ([FilePath], [InodeInfo], [(FilePath, Text)])
getDirContents' filtered config root = do
  contents <- getDirectoryContents root
  let blkdirs = if filtered
                then blacklistedDirs config
                else neverRecurseDirs
      allowed_names = filter (`notElem` blkdirs) contents
  (paths, skipped) <-
    foldM (\(acc, skip) path -> do
             stat <- getSymbolicLinkStatus $ root </> path
             case skipReason stat of
               Just reason -> return (acc, (path, reason):skip)
               Nothing -> do
                 let !ii = InodeInfo { inodeName  = path
                                     , inodeDirs  = []
                                     , inodeIsDir = isDirectory stat
                                     , inodeMTime = modificationTimeHiRes stat
                                     , inodeCTime = statusChangeTimeHiRes stat
                                     , inodeSize  = System.Posix.Files.fileSize stat
                                     }
                 ii' <- evaluate $!! ii
                 return (ii':acc, skip)
       ) ([], []) allowed_names
  let (dirs, files) = partition inodeIsDir paths
  return $!! (map inodeName dirs, files, skipped)

getDirContents :: Config -> FilePath
               -> IO ([FilePath], [InodeInfo], [(FilePath, Text)])
getDirContents = getDirContents' True

-- | Records the entries skipped while scanning as scan errors.
reportSkipped :: Ctx
              -> FilePath            -- ^ The directory holding the entries.
              -> [(FilePath, Text)]  -- ^ The skipped entries and reasons.
              -> IO ()
reportSkipped _ _ [] = return ()
reportSkipped ctx root skipped = do
  let errs = skippedErrors root skipped
  mapM_ (\(ProgressError item reason) ->
           ctxLogger ctx LevelWarn $ "Skipping " <> toLogStr item <>
             ": " <> toLogStr reason) errs
  atomically $ modifyTVar' (ctxScanProgress ctx) (incProgress errs 0 0)

-- | Converts skipped entries to scan errors.
skippedErrors :: FilePath -> [(FilePath, Text)] -> [ProgressError]
skippedErrors root =
  map (\(path, reason) -> ProgressError (Text.pack $ root </> path) reason)

-- | How a directory is handled when looking for picture folders.
data DirKind = PictureFolder  -- ^ A picture folder, loaded whole.
             | ContainerDir   -- ^ A directory searched for picture folders.
             | TooDeepDir     -- ^ A directory not searched, as too deep.
               deriving (Show, Eq)

-- | Classifies a directory found while looking for picture folders,
-- given its depth below the source or output directory (1 for the
-- latter's own sub-directories).
--
-- A directory whose name matches the directory regex is a picture
-- folder; any other directory is searched for picture folders, unless
-- at the maximum folder depth.
dirKind :: Config -> Int -> FilePath -> DirKind
dirKind config depth name
  | isOKDir config name = PictureFolder
  | depth < cfgMaxFolderDepth config = ContainerDir
  | otherwise = TooDeepDir

-- | The reason a directory is not searched for picture folders, if it
-- is too deep (see 'dirKind').
tooDeepReason :: Config -> Text
tooDeepReason config =
  "not a picture folder, and at the maximum folder depth (" <>
  tshow (cfgMaxFolderDepth config) <> "), so not searched"

-- | Scans one of the directories defined in the configuration.
scanBaseDir :: Ctx
            -> FilePath
            -> Bool
            -> IO [PicDir]
scanBaseDir ctx = scanContainer ctx 0

-- | Scans a directory for picture folders, at any depth down to the
-- maximum folder depth (see 'dirKind'). The names of the directories
-- above the picture folders are discarded and will not appear in the
-- final folder names; files directly in them are ignored.
scanContainer :: Ctx
              -> Int       -- ^ The directory's depth below the base dir.
              -> FilePath  -- ^ The directory to scan.
              -> Bool      -- ^ Whether it is under a source directory.
              -> IO [PicDir]
scanContainer ctx depth path isSource = do
  let config = ctxConfig ctx
      kind = dirKind config (depth + 1)
      ofKind k = filter ((== k) . kind)
  (dirs, _, skipped) <- getDirContents config path
  reportSkipped ctx path $
    skipped ++ map (, tooDeepReason config) (ofKind TooDeepDir dirs)
  -- Picture folders are loaded one at a time, but concurrently with
  -- the other directories.
  (folders, subdirs) <- concurrently
    (mapM (\d -> loadFolder ctx d (path </> d) isSource)
       (ofKind PictureFolder dirs))
    (mapConcurrently (\d -> scanContainer ctx (depth + 1) (path </> d) isSource)
       (ofKind ContainerDir dirs))
  return $ folders ++ concat subdirs

-- | Counts the items under a source or output directory, as
-- 'scanBaseDir' would scan them: the files in picture folders, and
-- the entries reported as scan errors at all levels.
countDir :: Config -> FilePath -> IO Int
countDir config = countContainer 0
  where
    countContainer depth path = do
      (dirs, _, skipped) <- getDirContents config path
      subdirs <- mapConcurrently (\d -> case dirKind config (depth + 1) d of
                                     PictureFolder -> countFolder (path </> d)
                                     ContainerDir  -> countContainer (depth + 1) (path </> d)
                                     TooDeepDir    -> return 1
                                 ) dirs
      return $!! sum subdirs + length skipped
    countFolder path = do
      (dirs, files, skipped) <- getDirContents config path
      subdirs <- mapConcurrently (countFolder . (path </>)) dirs
      return $!! sum subdirs + length files + length skipped

-- | Recursively count number of all files and directories under a directory.
--
-- This is used for the cache directory, where skipped entries are
-- counted (and handled) as files.
countDirRaw :: Config -> String -> IO Int
countDirRaw config path = do
  (dirpaths, iinfo, skipped) <- getDirContents' False config path
  subdirs <- mapConcurrently (\p -> countDirRaw config (path </> p)) dirpaths
  let total = sum subdirs + length iinfo + length skipped
  return $!! total

-- | Run action on all files under a given directory.
--
-- The skipped entries (e.g. symbolic links) are passed to the action
-- as well, like files.
allPathFiles :: Config -> FilePath -> (FilePath -> IO ()) -> FilePath -> IO ()
allPathFiles config root handler (makeRel -> subdir) = do
  let buildsub p = if subdir == "." then p else subdir </> p
  (dirpaths, iinfo, skipped) <- getDirContents' False config (root </> subdir)
  -- execute the action on all files in the current (subdir) directory.
  -- TODO: handle dirs vs just leaf? ignore dirs?
  mapM_ (\name -> handler (subdir </> name)) $
    map inodeName iinfo ++ map fst skipped
  -- recurse into subdirectories.
  mapM_ (allPathFiles config root handler . buildsub) dirpaths

addDirToRepo :: Config -> PicDir -> RepoDirs -> RepoDirs
addDirToRepo config dir =
  Map.insertWith (mergeFolders config) (pdName dir) dir

-- | Builds the filepath and filestatus pairs recursively for all
-- entries beneats a directory, and returns the skipped entries (with
-- the path relative to the directory) separately.
recursiveScanPath :: Config
                  -> FilePath
                  -> [FilePath]
                  -> IO ([InodeInfo], [(FilePath, Text)])
recursiveScanPath config root stack = do
  (!dirs, !files, !skipped) <- getDirContents config root
  let with_prefix = map (\ii -> ii { inodeDirs = stack }) files
      relpath p = System.FilePath.joinPath . reverse $ p:stack
      skipped' = map (first relpath) skipped
  subdirs <- mapM (\p -> recursiveScanPath config (root </> p)
                           (p:stack)) dirs
  return (with_prefix ++ concatMap fst subdirs,
          skipped' ++ concatMap snd subdirs)

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

-- | Split a path into basename and extension without the leading dot.
splitPathExt :: FilePath -> (FilePath, Text)
splitPathExt path =
  let (stem, ext') = splitExtension path
      ext = Text.pack $ case ext' of
        '.':v -> v
        _     -> ext'
  in (stem, ext)

isKnownMediaExt :: Config -> Text -> Bool
isKnownMediaExt config = (`Set.member` cfgAllMediaExts config)

isKnownMediaInode :: Config -> InodeInfo -> Bool
isKnownMediaInode config =
  isKnownMediaExt config . snd . splitPathExt . inodeFullName

-- | Builds a `File` object from its base `Inode` and other data.
mkFileFromInode :: SymbolizedItem -> InodeInfo -> Exif -> File
mkFileFromInode parent ii exif =
  File { fileName   = TS.fromString $ inodeName ii
        -- TODO: keep reverse, forward, symbolize?
        , fileDirs   = mkSymbolizedItem . System.FilePath.joinPath . reverse $ inodeDirs ii
        , fileCTime  = inodeCTime ii
        , fileMTime  = inodeMTime ii
        , fileSize   = inodeSize ii
        , fileParent = parent
        , fileExif   = exif
        }

implicitEventFromDateRange :: ShortText -> Maybe DateRange -> Maybe Event
implicitEventFromDateRange name range = do
  (start, end) <- range
  let days = diffDays (localDay end) (localDay start)
  guard $ days > 2
  pure $ if days >= 7
    then GrandVacationEvent { eventName = name, eventPeople = [], eventSource = EventImplicit implicitDateRangeDesc }
    else GetawayEvent { eventName = name, eventPeople = [], eventSource = EventImplicit implicitDateRangeDesc }

-- | Classifies a scanned inode into an image and any range shadows.
loadImage :: Config
          -> ShortText           -- ^ Folder name.
          -> SymbolizedItem      -- ^ Parent directory path.
          -> Bool                -- ^ Whether the file comes from a source directory.
          -> Map Text EExif      -- ^ Exif cache keyed by inode full name.
          -> InodeInfo           -- ^ Filesystem inode to classify.
          -> (Image, [Image])
loadImage config tname parent isSource lcache ii =
  let rawe = cfgRawExtsSet config
      side = cfgSidecarExts config
      jpeg = cfgJpegExts config
      move = cfgMovieExts config
      ewarn txt = def { exifWarning = Set.singleton txt }
      file_name = inodeFullName ii
      file_text = Text.pack file_name
      (base_full, ext) = splitPathExt file_name
      base_name = dropCopySuffix config base_full
      base_name_text = Text.pack base_name
      exif = case file_text `Map.lookup` lcache of
               Nothing         -> ewarn "Internal error: exif not read"
               Just (Left msg) -> ewarn $ "Cannot read exif: " `TS.append` TS.fromText msg
               Just (Right e)  -> e
      file_obj = mkFileFromInode parent ii exif
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
      snames = expandRangeFile config base_name_text
      range = case fromNullable snames of
                Nothing -> Nothing
                Just s  -> Just (ImageName . TS.fromText . head $ s, ImageName . TS.fromText . last $ s)
      flags = Flags {
        flagsSoftMaster = isSoftMaster
        }
      simgs = map (\expname ->
                     mkImage config (ImageName  (TS.fromText expname)) tname
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
        mkImage config (ImageName (TS.fromText base_name_text)) tname
        raw_file sidecar_file jpeg_file m_mov p_mov
        untrk range mtype flags
  in (img, if onlySidecar then [] else simgs)

-- | Builds a folder from already-scanned inodes and exif data.
buildFolderFromInodes :: Config
                      -> String              -- ^ Folder name.
                      -> FilePath            -- ^ Folder path on disk.
                      -> Bool                -- ^ Whether the folder is under a source directory.
                      -> [InodeInfo]         -- ^ Inodes belonging to the folder.
                      -> Map Text EExif      -- ^ Exif cache keyed by inode full name.
                      -> Maybe Event         -- ^ Event loaded from @corydalis.yaml@, if any.
                      -> PicDir
buildFolderFromInodes config name path isSource contents lcache yamlEvent =
  let tname = TS.fromString name
      dirpath = TS.fromString path
      parent = mkSymbolizedItem dirpath
      (known, otherFiles) = partition (isKnownMediaInode config) contents
      (images, shadows) =
        foldl' (\(images', shadows') f ->
                  let (img, newss) = loadImage config tname parent isSource lcache f
                  in (addImg config images' img, addImgs config shadows' newss)
               ) (Map.empty, Map.empty) known
      untracked = map (\ii -> mkFileFromInode parent ii def) otherFiles
      timestamp = Map.foldl' (\a img ->
                           (min <$> a <*> imageLocalDate img) <|>
                           a <|>
                           imageLocalDate img
                        ) Nothing images
      year = localDateToYear <$> timestamp
      exif = buildGroupExif images
      timesort = buildTimeSort images
      pstats = addUntrackedFiles untracked (computeImagesStats images)
      event = yamlEvent <|> implicitEventFromDateRange tname (sDateRange pstats)
  in PicDir { pdName = tname
            , pdMainPath = dirpath
            , pdSecPaths = []
            , pdImages = images
            , pdTimeSort = timesort
            , pdShadows = shadows
            , pdUntracked = untracked
            , pdYear = year
            , pdTimestamp = timestamp
            , pdExif = exif
            , pdStats = pstats
            , pdEvent = event }

-- | The name of the per-folder YAML file.
folderYamlName :: FilePath
folderYamlName = "corydalis.yaml"

-- | The per-folder YAML file to load, if any, among the folder's files
-- found by the scan: its name, or the reason not to load it.
--
-- As the scan only lists regular files (see 'skipReason'), the file is
-- not loaded if it is a symbolic link or a special file, which are
-- reported as for pictures; and since it is read whole, it is not
-- loaded if larger than 'maxYamlSize' (as found by the scan).
folderYamlFile :: [InodeInfo] -> Maybe (Either Text FilePath)
folderYamlFile contents = do
  ii <- find (\i -> null (inodeDirs i) && inodeName i == folderYamlName) contents
  return $ if toInteger (inodeSize ii) > toInteger maxYamlSize
             then Left $ "larger than " <> tshow maxYamlSize <> " bytes"
             else Right (inodeName ii)

-- | Builds a `PicDir` from an entire filesystem subtree.
loadFolder :: Ctx       -- ^ Scan context.
           -> String    -- ^ Folder name.
           -> FilePath  -- ^ Folder path on disk.
           -> Bool      -- ^ Whether the folder is under a source directory.
           -> IO PicDir
loadFolder ctx name path isSource = do
  let config = ctxConfig ctx
      scanProgress = ctxScanProgress ctx
      logfn = ctxLogger ctx
  logfn LevelDebug $ "loadFolder scan " <> toLogStr path
  (contents, skipped) <- recursiveScanPath config path []
  reportSkipped ctx path skipped
  logfn LevelDebug $ "loadFolder scanned " <> toLogStr path <> " (" <>
    toLogStr (show (length contents)) <> " inodes)"
  let known = filter (isKnownMediaInode config) contents
  (readexifs, lcache) <- getExif logfn (ctxScanToolLimits ctx) config path $
                           map inodeFullName known
  logfn LevelDebug $ "loadFolder yaml " <> toLogStr path
  let warnYaml reason = logfn LevelWarn $ "Folder " <> toLogStr path <>
        ": " <> toLogStr folderYamlName <> " ignored: " <> toLogStr reason
  yamlEvent <- case folderYamlFile contents of
    Nothing            -> return Nothing
    Just (Left reason) -> Nothing <$ warnYaml reason
    Just (Right fname) ->
      loadYaml (path </> fname) >>=
        either (\e -> Nothing <$ warnYaml (pack e :: Text)) (return . Just)
  logfn LevelDebug $ "loadFolder build " <> toLogStr path
  let totalitems = length contents
      noopexifs = max (totalitems - readexifs) 0
      folder = buildFolderFromInodes config name path isSource contents lcache yamlEvent
  logfn LevelDebug $ "loadFolder progress " <> toLogStr path
  atomically $ modifyTVar' scanProgress (incProgress [] noopexifs readexifs)
  logfn LevelDebug $ "loadFolder force " <> toLogStr path
  folder' <- evaluate $ force folder
  logfn LevelDebug $ "loadFolder done " <> toLogStr path
  return folder'

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

-- | The endings of the cache files' names in use: "-" and the suffix of
-- the configured sizes, the embedded images (and movie frames), and the
-- exif caches (see 'cachedBasename'). The dash is included, so that it
-- isn't added again for each cache file the cleanup checks.
--
-- A new kind of cache file must be added here, as the cache cleanup
-- deletes the files with any other suffix. The repository cache (see
-- 'repoDiskPath') isn't, as it is at the root of the cache directory,
-- which the cleanup doesn't visit.
cacheFileEndings :: Config -> [Text]
cacheFileEndings config =
  map ("-" <>) $
    "embedded" :
    map (Text.pack . show) (Set.toList (cfgAllImageSizes config)) ++
    ["exif", "bexif"]

-- | Whether a cache file is in use: its path is exactly that of a file
-- in the repository, followed by an ending in use (see
-- 'cacheFileEndings').
isLiveCacheFile :: [Text]    -- ^ The endings in use ('cacheFileEndings')
                -> Set Text  -- ^ The files in the repository (absolute paths)
                -> Text      -- ^ The cache file's path, without the cache
                             -- directory (so starting with a separator)
                -> Bool
isLiveCacheFile endings files path =
  any (\ending -> maybe False (`Set.member` files) $
                    Text.stripSuffix ending path) endings

-- | Checks if a given cache file is obsolete, and deletes it if so.
--
-- Only the cache files in use are kept (see 'isLiveCacheFile'); all the
-- others are deleted, e.g. those of files no longer in the repository,
-- those of sizes no longer configured, and the temporary files that
-- earlier development versions left next to the cache files when
-- interrupted. The cache files being written are in 'cacheTempDir',
-- which isn't cleaned up here.
examineCacheFile :: FilePath       -- ^ Absolute prefix path on the filesystem
                 -> TVar Progress  -- ^ For tracking progress
                 -> [Text]         -- ^ Endings of the cache files in use
                 -> Set Text       -- ^ Set of real image files (absolute paths)
                 -> LogFn          -- ^ Logger function
                 -> FilePath       -- ^ Relative path to the item being examined
                 -> IO ()
examineCacheFile prefix cleanProgress endings iset logger path = do
  let found = isLiveCacheFile endings iset (Text.pack $ pathSeparator:path)
  modifier <- if found
              then return incNoop
              else cleanCacheFile prefix logger path
  atomically $ modifyTVar' cleanProgress modifier

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
--
-- The goal of its progress is the number of files in these
-- directories, counted just before; as the cache can still change
-- meanwhile (e.g. previews built on demand), it is set to the number of
-- files actually examined once done, so that the results add up.
cleanupCache :: Ctx         -- ^ Context
             -> Repository  -- ^ Repository with current set of images
             -> [FilePath]  -- ^ Set of directories to cleanup
             -> IO Progress -- ^ Result of cleanup
cleanupCache ctx repo alldirs = do
  let cleanProgress = ctxCleanProgress ctx
      imgs = allRepoFiles repo
      iset = Set.fromList $ map (Text.pack . fileFullPath) imgs
      config = ctxConfig ctx
      cacheDir = cfgCacheDir config
      cachePaths = map (cacheDir </>) alldirs
      handler = examineCacheFile cacheDir cleanProgress
                  (cacheFileEndings config) iset (ctxLogger ctx)
  -- Reset before counting, which can take a while, so that the status
  -- page doesn't show the previous cleaning's results meanwhile.
  atomically $ writeTVar cleanProgress def
  ctxLogger ctx LevelInfo $ "Counting cache items under " <> toLogStr (show cachePaths)
  cachecount <- sum <$> mapConcurrently (countDirRaw config) cachePaths
  atomically . modifyTVar' cleanProgress $ \p -> p { pgGoal = cachecount }
  mapM_ (allPathFiles config cacheDir handler) alldirs
  atomically $ do
    modifyTVar' cleanProgress $ \p -> p { pgGoal = pgTotal p }
    readTVar cleanProgress

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
scanFSWrapper ctx newrepo =
  runScanAction ctx newrepo (scanFilesystem ctx newrepo)

runScanAction :: Ctx -> Repository -> IO Repository -> IO Repository
runScanAction ctx newrepo action =
  action `catch` \(err :: SomeException) ->
    tryUpdateRepo ctx newrepo {repoStatus = RepoError (sformat shown err)}

scanFilesystem :: Ctx -> Repository -> IO Repository
scanFilesystem ctx newrepo = do
  let logfn = ctxLogger ctx
      config = ctxConfig ctx
      scanProgress = ctxScanProgress ctx
  logfn LevelInfo "Launching scan filesystem"
  traceMarkerIO "scanFilesystem start"
  -- Only in profiling builds:
  -- _ <- incrementUserEra 1
  r1 <- tryUpdateRepo ctx (newrepo { repoStatus = RepoStarting })
  let srcdirs = map (, True)  (cfgSourceDirs config)
      outdirs = map (, False) (cfgOutputDirs config)
      alldirs = map fst $ srcdirs ++ outdirs
      alldirsAsRelative = map makeRel alldirs
  logfn LevelInfo $ "Counting images under " <> toLogStr (show alldirs)
  itemcounts <- mapConcurrently (countDir config) alldirs
  let cachePaths = map (cfgCacheDir config </>) alldirsAsRelative
  -- Ensure cache top dirs are created if missing (likely only during
  -- bootstrap, but better to always redo during scanning).
  mapConcurrently_ (createDirectoryIfMissing True) cachePaths
  atomically $ writeTVar scanProgress (def { pgGoal = sum itemcounts })
  start <- getZonedTime
  let ws = WorkStart { wsStart = start }
  r2 <- tryUpdateRepo ctx (r1 { repoStatus = RepoScanning { rsScanGoal = ws } })
  asyncDirs <- mapConcurrently (uncurry (scanBaseDir ctx))
                 $ srcdirs ++ outdirs
  logfn LevelInfo "Finished scanning directories"
  traceMarkerIO "scanFilesystem scanned dirs"
  scanned <- readTVarIO scanProgress
  end <- getZonedTime
  let repo = foldl' (flip (addDirToRepo config)) Map.empty $ concat asyncDirs
  let repo' = Map.map (resolveProcessedRanges config .
                       mergeShadows config) repo
      stats = computeRepoStats repo'
      rexif = repoGlobalExif repo'
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
  -- Reset the rendering's progress already: writing the disk cache can
  -- take a while, during which the status page would otherwise show
  -- the previous rendering's progress (forceBuildThumbCaches resets it
  -- again, as it can be called on its own).
  atomically . resetRenderProgress ctx . length $ renderableImages repo_as
  writeDiskCache config repo_as
  logfn LevelInfo "Finished building repo, starting rendering"
  traceMarkerIO "scanFilesystem start rendering"
  rendered <- forceBuildThumbCaches ctx repo_as
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
  traceMarkerIO "scanFilesystem start cleanup"
  clean_pg <- cleanupCache ctx repo_ar alldirsAsRelative
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
  traceMarkerIO "scanFilesystem done"
  return r4

-- | Computes the list of images that can be rendered.
renderableImages :: Repository -> [Image]
renderableImages =
  filter (not . null . allViewableImageFiles) .
  filterImagesByClass [ImageUnprocessed, ImageProcessed, ImageStandalone]

-- | Resets the rendering's progress: nothing rendered yet, and the
-- given number of renderable images (see 'renderableImages') left to
-- check (see 'forceBuildThumbCaches').
resetRenderProgress :: Ctx -> Int -> STM ()
resetRenderProgress ctx images = do
  writeTVar (ctxRenderProgress ctx) def
  -- Strict, so that a status request doesn't do the count.
  writeTVar (ctxRenderCheckProgress ctx) $! def { pgGoal = images }

-- | Pre-render all auto image sizes, converting only stale caches.
--
-- First classifies every image×size, checking only the cache files
-- (see 'imagePendingSizes'; no tool runs), then builds the stale ones,
-- including any extraction they need. Render progress 'pgGoal' is the
-- pending work, not the classified total; the classification has its
-- own progress ('ctxRenderCheckProgress'), counting the images
-- checked.
forceBuildThumbCaches :: Ctx -> Repository -> IO Progress
forceBuildThumbCaches ctx repo = do
  checkRepoOwnership ctx repo
  let config = ctxConfig ctx
      renderProgress = ctxRenderProgress ctx
      checkProgress = ctxRenderCheckProgress ctx
      images = renderableImages repo
      total = length images
      sizes = Set.toList (cfgAutoImageSizes config)
      imageForError i = sformat (stext % "/" % stext % " at resolution " % int)
                         (TS.toText $ imgParent i) (TS.toText . unImageName . imgName $ i)
      -- The last image is counted along with setting the rendering's
      -- goal (below), so that the check is never complete before the
      -- goal is set.
      classifyImage (pend, n, es) (k, i) = do
        checkRepoOwnership ctx repo
        res <- imagePendingSizes (ctxLogger ctx) config i sizes
        when (k < total) . atomically $ modifyTVar' checkProgress incDone
        return $ case res of
          Left err ->
            let msg = Text.pack $ show err
            in (pend, n, [ProgressError (imageForError i size) msg | size <- reverse sizes] ++ es)
          Right stale ->
            (map (i,) (reverse stale) ++ pend, n + length sizes - length stale, es)
  atomically $ resetRenderProgress ctx total
  (pending, noops, errs) <- foldM classifyImage ([], 0, []) (zip [1..] images)
  -- The check is marked complete explicitly, not relying on its count.
  atomically $ do
    writeTVar renderProgress Progress
      { pgErrors = errs
      , pgNoop = noops
      , pgDone = 0
      , pgGoal = length pending + length errs
      }
    modifyTVar' checkProgress $ \p -> p { pgDone = pgGoal p }
  mapM_ (\(i, size) -> do
            checkRepoOwnership ctx repo
            res <- imageAtRes (ctxRenderToolLimits ctx) (ctxLogger ctx) config i . Just . ImageSize $ size
            let modifier = case res of
                  Left   err      -> incErrors (imageForError i size) (Text.pack $ show err)
                  Right (_, _, _) -> incDone
            atomically $ modifyTVar' renderProgress modifier
        ) pending
  readTVarIO renderProgress

repoDiskFile :: String
repoDiskFile = "repo"

repoDiskPath :: Config -> FilePath -> FilePath
repoDiskPath config path = cachedBasename config path "cache"

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

-- | Counts images by an attribute, skipping the images without it.
--
-- Returns the counts and their attributes, the most frequent first (and,
-- for equal counts, the attributes in descending order).
countImagesBy :: Ord a => (Image -> Maybe a) -> [Image] -> [(Int64, a)]
countImagesBy attr =
  sortBy (comparing Down) . map (\(a, b) -> (b, a)) . Map.toList .
  foldl' (\m i -> maybe m (\a -> Map.insertWith (+) a 1 m) (attr i)) Map.empty

-- | The earliest and the latest of the images with a creation date,
-- each with its date and the given attribute.
--
-- Images with the same creation time are ordered by the attribute.
imageDateRange :: Ord a => (Image -> a) -> [Image]
               -> Maybe ((ExifTime, a), (ExifTime, a))
imageDateRange attr images =
  let dated = sort [ (cd, attr i) | i <- images
                                  , Just cd <- [exifCreateDate (imgExif i)] ]
  in fromNullable dated >>= (\nn -> Just (head nn, last nn))

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

-- | Whether a cache file is missing, empty, or older than the source mtime.
--
-- An empty file is never a valid image. Such files were left behind by
-- failed conversions (before previews were written via a temporary
-- file), and by convert succeeding without writing the file it was
-- given (before previews were checked, see 'buildPreviewFile'); they
-- are rebuilt.
--
-- A file that can't be checked at all (e.g. as its name is too long,
-- or its parent directory is a regular file) also needs building, so
-- that the build reports the problem, the way its callers report
-- errors, instead of the check throwing.
cacheFileNeedsBuild :: FilePath -> POSIXTime -> IO Bool
cacheFileNeedsBuild fpath mtime = isNothing <$> upToDateCacheFile fpath mtime

-- | The last time an up to date cache file was touched (see
-- 'filePathLastTouch'), or 'Nothing' if it needs building (see
-- 'cacheFileNeedsBuild'), from a single look at the file, so that it
-- can't change in between.
upToDateCacheFile :: FilePath -> POSIXTime -> IO (Maybe POSIXTime)
upToDateCacheFile fpath mtime = do
  stat <- tryIO $ getFileStatus fpath
  return $ case stat of
    Right st | modificationTimeHiRes st >= mtime,
               System.Posix.Files.fileSize st /= 0 ->
      Just $ modificationTimeHiRes st `max` statusChangeTimeHiRes st
    _ -> Nothing

-- | Whether a scaled preview for this request size needs generating.
--
-- Snaps @size@ through 'findBestSize' onto 'cfgAllImageSizes' so the
-- check uses the same cache path as 'loadCachedOrBuild'. For auto
-- sizes (already in that set) the snap is identity. Returns 'False'
-- when no cached size applies (request smaller than every configured
-- size).
scaledCacheNeedsBuild :: Config -> FilePath -> POSIXTime -> ImageSize -> IO Bool
scaledCacheNeedsBuild config origPath mtime size =
  case findBestSize size (cfgAllImageSizes config) of
    Nothing  -> return False
    Just res -> cacheFileNeedsBuild (scaledImagePath config origPath res) mtime

-- | The sizes, among the given ones, whose previews of this image need
-- (re)building, checking only the cache files.
--
-- No tool is run (hence no 'ToolLimits'): for a raw file or a movie
-- whose viewable version must be extracted (see 'imageViewSource'), a
-- missing or stale extracted file means all the sizes need building,
-- and the extraction is left to the build. Otherwise, each size needs
-- building if its preview is missing, or older than the viewable file.
imagePendingSizes :: LogFn -> Config -> Image -> [Int] -> IO (Either ImageError [Int])
imagePendingSizes logfn config img sizes = try $ do
  (f, kind) <- maybe (throwIO ImageNotViewable) return $ imageViewSource config img
  ensureStorageFile logfn config f
  let path = fileFullPath f
      staleSizes mtime =
        filterM (scaledCacheNeedsBuild config path mtime . ImageSize) sizes
  case kind of
    ViewAsIs -> staleSizes (fileLastTouch f)
    _ -> upToDateCacheFile (embeddedImagePath config path) (fileLastTouch f)
           >>= maybe (return sizes) staleSizes

-- | Generate a preview for an image.
--
-- In this context, a preview is a smaller version of an image, for
-- faster/optimised use on a lower-powered device (mobile, etc.). It
-- is done using imagemagick's convert tool.
--
-- TODO: add loging of failures.
-- TODO: improve path manipulation \/ concatenation.
-- The decoder is passed explicitly to convert, since otherwise
-- ImageMagick picks it based on the file contents, e.g. running a
-- PostScript or SVG document named @photo.jpg@ as such. If no decoder
-- is known for the file (the 'Left' case), the preview can't be built.
loadCachedOrBuild :: ToolLimits -> Config -> FilePath -> FilePath -> Either Text String
                  -> MimeType -> POSIXTime -> ImageSize
                  -> IO (Bool, MimeType, FilePath)
loadCachedOrBuild tools config origPath bytesPath decoder mime mtime size = do
  let res = findBestSize size (cfgAllImageSizes config)
  case res of
    Nothing -> return (False, mime, bytesPath)
    Just res' -> do
      let fpath = scaledImagePath config origPath res'
          kind = if res' <= cfgThumbnailSize config
                   then PreviewThumbnail
                   else PreviewScaled
      -- Note: this calls getFileStatus not getSymbolicLinkStatus
      -- since this is a file, not a directory, so symlinks to
      -- somewhere else are OK-ish; we do not recurse into the target
      -- directory.
      needsGen <- cacheFileNeedsBuild fpath mtime
      when needsGen $ do
        decoder' <- either (throwIO . ImageError) return decoder
        buildPreviewFile tools config kind fpath bytesPath
          =<< convertProcess config (previewArgs kind decoder' res')
      return (needsGen, previewMimeType kind, fpath)

-- | Builds a preview into the given cache file, running the given tool
-- with the picture as its standard input, and its standard output going
-- to a temporary file (see 'previewArgs').
--
-- The tool's slot is taken first (see 'withToolSlot'), so that the
-- builds waiting for one hold neither the temporary file nor the
-- picture open.
--
-- The temporary file is moved in place only if the tool succeeded and
-- the file starts with the signature of the expected format (see
-- 'checkPreviewImage'): a failed, empty or partial preview must not be
-- served. If the tool fails, or its preview is refused, its error
-- output is added to the error.
--
-- All failures are thrown as an 'ImageError', which is what the
-- callers expect: any other exception (e.g. if the temporary file
-- can't be created, as the picture's name is too long once the cache
-- suffix is added) would fail the request, or stop the rendering of all
-- the other pictures (see 'toImageError').
buildPreviewFile :: ToolLimits -> Config -> PreviewFormat
                 -> FilePath  -- ^ The cache file to build.
                 -> FilePath  -- ^ The picture.
                 -> ProcessConfig stdin stdout stderr  -- ^ The tool.
                 -> IO ()
buildPreviewFile tools config kind fpath input pconfig =
  either (throwIO . toImageError) return =<<
  tryAny (withToolSlot tools $ \slot -> writeViaTempFile config fpath $ \tmpPath -> do
    let openInput = openRegularFile input `catchIO`
          (throwIO . ImageError . sformat ("Cannot read the image to scale: " % shown))
    (exitCode, err) <- either (throwIO . ImageError) return =<<
      bracket openInput System.IO.hClose (\h ->
        runToolToFile slot tmpPath (setStdin (useHandleOpen h) pconfig))
    let output = Text.strip $ decodeToolOutput err
        withOutput msg | Text.null output = msg
                       | otherwise = msg <> ": " <> output
    unless (exitCode == ExitSuccess) . throwIO . ImageError . withOutput $
      sformat ("convert failed (" % shown % ")") exitCode
    checkPreviewFile kind tmpPath >>= either (throwIO . ImageError . withOutput) return)

-- | Opens a regular file for reading, refusing anything else.
--
-- The file is opened without waiting: opening a pipe for reading would
-- otherwise wait for a writer, with no time limit (e.g. if a picture
-- was replaced by one after the scan). Its type is then checked on the
-- open file itself, so that it can't be replaced in between.
--
-- The handle is in binary mode, as 'fdToHandle' makes it. Asynchronous
-- exceptions are masked throughout, so that the file descriptor is
-- closed exactly once: here if the handle wasn't made, and otherwise by
-- the handle, which owns it. (Opening and checking the file are system
-- calls, which can't be interrupted anyway, and don't wait.)
openRegularFile :: FilePath -> IO System.IO.Handle
openRegularFile path = mask_ $ do
  fd <- openFd path ReadOnly defaultFileFlags { nonBlock = True, cloexec = True }
  (do status <- getFdStatus fd
      unless (isRegularFile status) . throwIO . ImageError $
        sformat ("Cannot read the image to scale: " % string % " is not a regular file") path
      fdToHandle fd) `onException` closeFd fd

-- | The kinds of previews built by convert.
data PreviewFormat
  = PreviewThumbnail  -- ^ A thumbnail: a PNG, scaled down and padded
                      -- to a square with transparency.
  | PreviewScaled     -- ^ A scaled-down (never enlarged) image: a JPEG.
  deriving (Show, Eq)

-- | The ImageMagick coder for a preview's output.
--
-- These are the canonical coder names, as the ImageMagick policy lists
-- them: ImageMagick 6 checks the name as given, so e.g. @jpg@ would be
-- refused by a policy allowing only @JPEG@.
previewCoder :: PreviewFormat -> String
previewCoder PreviewThumbnail = "png"
previewCoder PreviewScaled    = "jpeg"

-- | The MIME type of a preview.
previewMimeType :: PreviewFormat -> MimeType
previewMimeType PreviewThumbnail = "image/png"
previewMimeType PreviewScaled    = "image/jpeg"

-- | The bytes a preview of the given kind starts with.
previewMagic :: PreviewFormat -> LByteString
previewMagic PreviewThumbnail = pngMagic
previewMagic PreviewScaled    = jpegMagic

-- | The convert arguments for building a preview, reading the picture
-- from the standard input, and writing the preview to the standard
-- output (see 'buildPreviewFile').
--
-- Only the first image of the input is used (the @[0]@): files can
-- hold several (e.g. the pages of a TIFF, the frames of an animated
-- WebP, or the images of a HEIC file, which ImageMagick returns with
-- the primary image first), and convert would otherwise try to write
-- each of them, to numbered files. (For an animated WebP, ImageMagick
-- still decodes all the frames, so the policy must not limit how many
-- images it reads from a file; see config\/magick\/policy.xml.)
--
-- No path is named on the command line, so that convert has none to
-- interpret: it expands format specifiers in file names (e.g. the @%d@
-- of a picture named @100%d.jpg@, or of a folder name), in the output
-- name, and, once an image is selected, in the input name too; and
-- for several images, it writes numbered files next to the output file
-- it was given, not that file. Telling it to use the names as given
-- (@filename:literal@) is no fix, as ImageMagick 7.1.2 then also takes
-- the @[0]@ as part of the name.
--
-- The standard input and output are named as @\/dev\/fd\/0@ and
-- @\/dev\/fd\/1@, which convert opens as ordinary files. Not @-@: with
-- it, convert first copies the whole input to a temporary file (left
-- behind if it is killed). The output must be a file, not a pipe, to
-- which convert fails to write. @\/dev\/stdout@ would work too, but was
-- seen as a regular file, not a link, in a container whose
-- @\/dev\/fd@ was fine. systemd's @PrivateDevices@ keeps @\/dev\/fd@.
--
-- @-thumbnail@ records the name of its input in the thumbnail (its
-- @Thumb::URI@ property); here, it only names @\/dev\/fd\/0@, but it
-- is removed anyway, as it would disclose the picture's path to anyone
-- who can see the thumbnails if a path were ever given.
previewArgs :: PreviewFormat  -- ^ The kind of preview.
            -> String         -- ^ The decoder for the input.
            -> Int            -- ^ The size to scale down to (if larger).
            -> [String]
previewArgs kind decoder size =
  [decoder ++ ":/dev/fd/0[0]"] ++ operators ++ [previewCoder kind ++ ":/dev/fd/1"]
  where geom = show size ++ "x" ++ show size ++ ">"
        operators = case kind of
          PreviewThumbnail -> ["-thumbnail", geom, "-background", "none",
                               "-gravity", "center", "-extent", geom,
                               "+set", "Thumb::URI"]
          PreviewScaled    -> ["-resize", geom]

-- | Checks a preview written by convert, from its first bytes.
--
-- A safeguard: convert has been seen to succeed without writing the
-- file it was given (for several images, before only the first was
-- used, see 'previewArgs'); an empty preview, or one that doesn't
-- start with the signature of the format it is served as, must not be
-- cached.
checkPreviewImage :: PreviewFormat
                  -> LByteString  -- ^ The preview's first bytes (as many
                                  -- as its 'previewMagic' has, or all of
                                  -- them if it is shorter).
                  -> Either Text ()
checkPreviewImage kind start
  | BSL.null start = Left "Building the preview produced no image"
  | not (previewMagic kind `BSL.isPrefixOf` start) =
      Left $ sformat ("Building the preview produced something other than a " %
                      string % " image") (toUpper $ previewCoder kind)
  | otherwise = Right ()

-- | Checks the preview written by convert to the given file (see
-- 'checkPreviewImage').
checkPreviewFile :: PreviewFormat -> FilePath -> IO (Either Text ())
checkPreviewFile kind path =
  checkPreviewImage kind . snd <$> readFileStart path (BSL.length $ previewMagic kind)

-- | The 'ImageError' for a failure to build a cache file: the exception
-- itself if it is one, or else the exception, shown after \"Failed to
-- write the cache file: \" (e.g. if the temporary file couldn't be
-- created).
toImageError :: SomeException -> ImageError
toImageError e =
  fromMaybe (ImageError $ sformat ("Failed to write the cache file: " % shown) e)
            (fromException e)

-- | Runs an action writing a cache file, returning the message of its
-- failure, if any (see 'toImageError').
--
-- Only synchronous exceptions are caught ('tryAny'), so that e.g. a
-- cancelled request still stops the action.
tryImageError :: IO () -> IO (Either Text ())
tryImageError action = do
  result <- tryAny action
  return $ case result of
    Left e -> Left $ case toImageError e of
      ImageError msg   -> msg
      ImageNotViewable -> "The image is not viewable"
    Right () -> Right ()

-- | Returns the size of a file and its first bytes: the given number of
-- them, or the whole file if it is shorter.
readFileStart :: FilePath -> Int64 -> IO (Integer, LByteString)
readFileStart path n =
  System.IO.withBinaryFile path System.IO.ReadMode $ \h ->
    (,) <$> System.IO.hFileSize h
        <*> (fromStrict <$> BS.hGet h (fromIntegral n))

-- | Creates a cache file by writing it to a temporary file in the
-- cache's temporary directory (see 'cacheTempDir'), then renaming it to
-- its final name, so that the file only appears once fully written.
-- Both directories are created first, if needed; as the file is
-- renamed, it must be on the cache directory's filesystem.
--
-- The temporary file is named after the start of the file's name (its
-- first 50 characters, at most 200 bytes), which shows what is being
-- written, while leaving room for the random part added to it: with
-- the whole name, a file whose name is valid, but near the length
-- limit, couldn't be written.
--
-- If the writing action fails, the temporary file is removed, and the
-- exception is rethrown. The file gets the default permissions (subject
-- to the umask), as a newly created file would; if it replaces an
-- existing file, it does not keep that file's permissions (the cache
-- files are all created by this code, so they have the default ones).
writeViaTempFile :: Config -> FilePath -> (FilePath -> IO ()) -> IO ()
writeViaTempFile config path write = do
  let (dir, name) = splitFileName path
      tmpDir = cacheTempDir config
  createDirectoryIfMissing True dir
  createDirectoryIfMissing True tmpDir
  bracketOnError
    (do (tmpPath, h) <- System.IO.openTempFileWithDefaultPermissions tmpDir
                          (take 50 name ++ ".tmp")
        System.IO.hClose h
        return tmpPath)
    (void . tryIO . removeFile)
    (\tmpPath -> write tmpPath >> renameFile tmpPath path)

-- | Builds the process configuration for running ImageMagick's convert.
--
-- If a policy directory is configured, ImageMagick is pointed to it via
-- the MAGICK_CONFIGURE_PATH environment variable. convert gets none of
-- Corydalis' other open files.
convertProcess :: Config -> [String] -> IO (ProcessConfig () () ())
convertProcess config args = do
  let pconfig = setCloseFds True $ proc "convert" args
      var = "MAGICK_CONFIGURE_PATH"
  case cfgMagickPolicyDir config of
    Nothing -> return pconfig
    Just dir -> do
      env <- System.Environment.getEnvironment
      return $ setEnv ((var, dir) : filter ((/= var) . fst) env) pconfig

-- | Checks that ImageMagick uses the configured policy.
--
-- ImageMagick silently ignores a policy it can't find, so this checks
-- that the policy file exists, and that ImageMagick lists it among the
-- policies it loaded. Without a configured policy directory, there is
-- nothing to check.
checkMagickPolicy :: Config -> IO (Either Text ())
checkMagickPolicy config =
  case cfgMagickPolicyDir config of
    Nothing -> return $ Right ()
    Just dir -> do
      let policyFile = dir </> "policy.xml"
          failWith = return . Left
      found <- doesFileExist policyFile
      if not found
        then failWith $ sformat ("ImageMagick policy file " % string % " not found") policyFile
        else do
          expected <- canonicalizePath policyFile
          pconfig <- convertProcess config ["-list", "policy"]
          result <- readToolProcess pconfig
          case result of
            Left msg -> failWith msg
            Right (ExitSuccess, out, _) -> do
              let loaded = [ Text.unpack (Text.strip path)
                           | line <- Text.lines (decodeToolOutput out)
                           , Just path <- [Text.stripPrefix "Path:" (Text.strip line)] ]
              loaded' <- mapM canonicalizePath loaded
              return $ if expected `elem` loaded'
                then Right ()
                else Left $ sformat ("ImageMagick did not load the policy file " % string) policyFile
            Right (_, out, err) ->
              failWith $ "Failed to list the ImageMagick policies: " <>
                decodeToolOutput (err `BSL.append` out)

-- | Ordered list of tags that represent embedded images.
previewTags :: [String]
previewTags = ["JpgFromRaw", "PreviewImage"]

-- | Minimum thumbnail size to consider it a valid image.
minImageSize :: Int64
minImageSize = 512

-- | The bytes every JPEG image starts with.
--
-- A JPEG stream must start with the start of image (SOI) marker, @FF
-- D8@, followed by another marker (e.g. an application or table
-- segment); all markers, and the fill bytes allowed before them, start
-- with @FF@.
jpegMagic :: LByteString
jpegMagic = BSL.pack [0xFF, 0xD8, 0xFF]

-- | The bytes every PNG image starts with (its signature).
pngMagic :: LByteString
pngMagic = BSL.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

-- | Checks the result of extracting an embedded image with exiftool.
--
-- Succeeds if the extraction succeeded, and the image is a JPEG large
-- enough to be a real image. Exiftool already checks this for most,
-- but not all, embedded images it knows about. Note that exiftool
-- succeeds, without any output, if the image doesn't have the
-- requested embedded image.
checkEmbeddedImage :: String       -- ^ The embedded image's tag.
                   -> ExitCode     -- ^ Exiftool's exit code.
                   -> Int64        -- ^ The image's size.
                   -> LByteString  -- ^ The image's first bytes (at least
                                   -- as many as 'jpegMagic' has).
                   -> LByteString  -- ^ Exiftool's error output.
                   -> Either Text ()
checkEmbeddedImage tag exitCode size start err
  | exitCode /= ExitSuccess =
      Left $ sformat ("Failed to extract the embedded image " % string % " (" %
                      shown % "): " % stext) tag exitCode (decodeToolOutput err)
  | size == 0 =
      Left $ sformat ("No embedded image " % string % " found") tag
  | size < minImageSize =
      Left $ sformat ("The embedded image " % string % " is too small (" % int %
                      " bytes)") tag size
  | not (jpegMagic `BSL.isPrefixOf` start) =
      Left $ sformat ("The embedded image " % string % " is not a JPEG") tag
  | otherwise = Right ()

-- | The exiftool arguments for extracting the given embedded image.
--
-- The path comes after @--@, so that it is never taken as an option,
-- even if it starts with a dash (the paths we pass are absolute, but
-- this makes it independent of that, as for the scan).
embeddedArgs :: String -> FilePath -> [String]
embeddedArgs tag path = ["-b", "-" ++ tag, "--", path]

-- | Extracts and saves an embedded thumbnail from an image.
--
-- The extracted image is required to be a JPEG, otherwise it is not
-- saved, and an error is returned. A previously extracted image is
-- reused, unless it is older than the given modification time of the
-- source file.
extractEmbedded :: ToolLimits -> Config -> FilePath -> POSIXTime -> String
                -> IO (Either Text (MimeType, FilePath))
extractEmbedded tools config path mtime tag = do
  let outputPath = embeddedImagePath config path
      args = embeddedArgs tag path
      mime = "image/jpeg"
      goodRet = Right (mime, outputPath)
  needsBuild <- cacheFileNeedsBuild outputPath mtime
  if needsBuild
    then do
      -- Exiftool writes the image straight to a temporary file, moved
      -- in place only if it is a valid image; otherwise, or if the
      -- extraction is interrupted, it is removed, so that no partial
      -- or invalid image is served as the preview.
      let pconfig =
            setStdin closed
            . setCloseFds True
            $ proc "exiftool" args
      result <- tryImageError . withToolSlot tools $ \slot ->
        writeViaTempFile config outputPath $ \tmpPath -> do
          ran <- runToolToFile slot tmpPath pconfig
          (exitCode, err) <- either (throwIO . ImageError) return ran
          (size, start) <- readFileStart tmpPath (BSL.length jpegMagic)
          either (throwIO . ImageError) return $
            checkEmbeddedImage tag exitCode (fromIntegral size) start err
      return (const goodRet =<< result)
    else
      return goodRet

-- | Extract the first valid thumbnail from an image.
bestEmbedded :: ToolLimits -> Config -> FilePath -> POSIXTime -> IO (Either Text (MimeType, FilePath))
bestEmbedded tools config path mtime =
  go config path previewTags
  where go _ _ [] = return $ Left "No tags available"
        go c p (x:xs) = do
          r <- extractEmbedded tools c p mtime x
          case r of
            Left _ -> case xs of
              [] -> return r
              _  -> go c p xs
            Right _ -> return r

-- | Checks is a file is directly viewable.
viewableAsIs :: ShortText -> Config -> Bool
viewableAsIs (TS.toText -> path) (cfgViewableImages -> exts)  =
  any (`Text.isSuffixOf` path) exts

-- | The ffmpeg arguments for extracting the first frame of a movie, as
-- a JPEG.
--
-- The demuxer is passed explicitly, since otherwise ffmpeg guesses the
-- format from the file contents, and some formats (e.g. playlists, or
-- concat scripts) make it read other files; it also only reads local
-- files, and the "file:" prefix ensures the path is read as a file
-- name, even if it contains a colon.
firstFrameArgs :: String    -- ^ The demuxer (see 'movieDemuxer').
               -> FilePath  -- ^ The movie.
               -> FilePath  -- ^ The output file.
               -> [String]
firstFrameArgs demuxer path output = [
  "-nostdin",
  -- Only errors in its output, which is shown when the extraction
  -- fails: by default, ffmpeg also writes its banner, the details of
  -- the streams, and its progress.
  "-loglevel", "error",
  "-y",
  "-protocol_whitelist", "file",
  "-f", demuxer,
  "-i", "file:" ++ path,
  "-r", "1",
  "-frames", "1",
  "-f", "image2",
  -- Use the output name as given, not as a pattern (e.g. if it
  -- contains '%').
  "-update", "1",
  -- The output file has no extension, so ffmpeg would use the default
  -- codec of the muxer, which happens to be mjpeg; set it explicitly
  -- instead.
  "-c:v", "mjpeg",
  output
  ]

-- | Extracts and saves the first frame from a movie, as a JPEG.
--
-- As for embedded images, a previously extracted frame is reused,
-- unless it is older than the given modification time of the movie.
extractFirstFrame :: ToolLimits -> Config -> FilePath -> POSIXTime
                  -> IO (Either Text (MimeType, FilePath))
extractFirstFrame tools config path mtime =
  case movieDemuxer config path of
    Left msg -> return $ Left msg
    Right demuxer -> do
      let outputPath = embeddedImagePath config path
          mime = "image/jpeg"
          goodRet = Right (mime, outputPath)
      needsBuild <- cacheFileNeedsBuild outputPath mtime
      if needsBuild
        then do
          -- ffmpeg can leave a partial file behind when it fails (or is
          -- interrupted), so it writes to a temporary file, moved in
          -- place only on success.
          result <- tryImageError . withToolSlot tools $ \slot ->
            writeViaTempFile config outputPath $ \tmpPath -> do
              let pconfig =
                    setStdin closed
                    . setCloseFds True
                    $ proc "ffmpeg" (firstFrameArgs demuxer path tmpPath)
              ran <- runTool slot pconfig
              case ran of
                Left msg -> throwIO $ ImageError msg
                Right (exitCode, out, err) ->
                  unless (exitCode == ExitSuccess) .
                    throwIO . ImageError . decodeToolOutput $ out `BSL.append` err
          return (const goodRet =<< result)
        else
          return goodRet

-- | Compute a presumed jpeg's mime type.
jpegMimeType :: File -> MimeType
jpegMimeType = fileMimeType "image/jpeg"

-- | The ImageMagick decoder for a file, based on its extension.
imageDecoder :: Config -> FilePath -> Either Text String
imageDecoder config path =
  let ext = Text.toLower . Text.pack . drop 1 . takeExtension $ path
  in case lookup ext (cfgImageDecoders config) of
       Just decoder -> Right (Text.unpack decoder)
       Nothing -> Left $ sformat ("No image decoder configured for extension '" % stext %
                                  "' (see the imagedecoders setting)") ext

-- | The ffmpeg demuxer for a movie, based on its extension.
movieDemuxer :: Config -> FilePath -> Either Text String
movieDemuxer config path =
  let ext = Text.toLower . Text.pack . drop 1 . takeExtension $ path
  in case lookup ext (cfgMovieDemuxers config) of
       Just demuxer -> Right (Text.unpack demuxer)
       Nothing -> Left $ sformat ("No movie demuxer configured for extension '" % stext %
                                  "' (see the moviedemuxers setting)") ext

-- | The ImageMagick decoder for images extracted by Corydalis.
--
-- Both embedded previews and movie frames are extracted as JPEGs.
extractedDecoder :: Either Text String
extractedDecoder = Right "jpeg"

-- | Select best movie for an image.
bestMovie :: Image -> Maybe File
bestMovie img =
  case imgMovs img of
    m:_ -> Just m
    _   -> imgMasterMov img

-- | Ensures that a file can be read, i.e. it's still within the
-- configured picture directories, without symbolic links.
--
-- The scan skips symbolic links, but a file could have been replaced
-- by one (or one of its parent directories) since then.
--
-- The error shown to users is generic, since a rescan fixes it (the
-- file is then either used normally, or skipped and listed as a scan
-- error); the exact reason is logged.
ensureStorageFile :: LogFn -> Config -> File -> IO ()
ensureStorageFile logfn config f = do
  let path = fileFullPath f
  ok <- isCanonicalStoragePath config path
  unless ok $ do
    logfn LevelWarn $ "File " <> toLogStr path <> " has changed since " <>
      "the last scan: it is no longer within the picture folders, or its " <>
      "path now contains a symbolic link"
    throwIO $ ImageError
      "The file has changed since the last scan; a rescan will update it."

-- | How the viewable version of an image is obtained from its file
-- (see 'imageViewSource').
data ViewKind
  = ViewAsIs     -- ^ The file itself (a JPEG, or a viewable raw file).
  | ViewEmbedded -- ^ A preview embedded in the (raw) file.
  | ViewMovie    -- ^ A movie's embedded preview, or else its first frame.
  deriving (Show, Eq)

-- | The file an image is viewed from, and how its viewable version is
-- obtained from it: a processed file is used directly; for a raw file,
-- its embedded preview is extracted (unless browsers can show it as it
-- is); for a movie, its embedded preview or first frame.
--
-- This only picks the file; 'getViewableVersion' does the extraction,
-- and 'imagePendingSizes' only checks what it would need.
imageViewSource :: Config -> Image -> Maybe (File, ViewKind)
imageViewSource config img
  | j:_ <- imgJpegPath img = Just (j, ViewAsIs)
  | Just j <- imgRawPath img,
    True <- flagsSoftMaster (imgFlags img) = Just (j, ViewAsIs)
  | Just r <- imgRawPath img =
      Just (r, if fileName r `viewableAsIs` config then ViewAsIs else ViewEmbedded)
  | Just m <- bestMovie img = Just (m, ViewMovie)
  | otherwise = Nothing

-- | Returns a viewable version of an image (see 'imageViewSource'),
-- extracting it if needed. Also returns the ImageMagick decoder to use
-- for the viewable file.
getViewableVersion :: ToolLimits -> LogFn -> Config -> Image
                   -> IO (File, FilePath, Either Text String, MimeType, POSIXTime)
getViewableVersion tools logfn config img = do
  (f, kind) <- maybe (throwIO ImageNotViewable) return $ imageViewSource config img
  ensureStorageFile logfn config f
  let path = fileFullPath f
      extracted = either (throwIO . ImageError) $ \(mime, epath) -> do
        -- The file can disappear meanwhile (e.g. removed from the cache
        -- directory by hand): a failure for this image only.
        mtime <- either (const . throwIO $ ImageError "The extracted preview can't be read") return
                   =<< tryIO (filePathLastTouch epath)
        return (f, epath, extractedDecoder, mime, mtime)
  case kind of
    ViewAsIs ->
      return (f, path, imageDecoder config path, jpegMimeType f, fileLastTouch f)
    ViewEmbedded ->
      extracted =<< bestEmbedded tools config path (fileLastTouch f)
    ViewMovie -> do
      embedded <- bestEmbedded tools config path (fileLastTouch f)
      extracted =<< case embedded of
        Left _  -> extractFirstFrame tools config path (fileLastTouch f)
        Right e -> return (Right e)

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
imageAtRes :: ToolLimits -> LogFn -> Config -> Image -> Maybe ImageSize -> IO (Either ImageError (Bool, MimeType, FilePath))
imageAtRes tools logfn config img size = try $ do
  (origFile, path, decoder, mime, mtime) <- getViewableVersion tools logfn config img
  case size of
    Nothing -> return (False, mime, path)
    Just s  -> loadCachedOrBuild tools config (fileFullPath origFile) path decoder mime mtime s

-- | Returns the movie file to serve for an image, after checking that
-- it can be read (see 'ensureStorageFile').
movieToServe :: LogFn -> Config -> Image -> IO (Either ImageError File)
movieToServe logfn config img = try $
  case bestMovie img of
    Just f  -> f <$ ensureStorageFile logfn config f
    Nothing -> throwIO ImageNotViewable

imgProblems :: Image -> Set ShortText
imgProblems =
  Set.map ("exif: " <>) . exifWarning . imgExif

pdProblems :: PicDir -> NameStats ShortText
pdProblems =
  foldl' (\m probs ->
            if null probs
              then Map.insertWith (+) Nothing 1 m
              else foldl' (\m' e -> Map.insertWith (+) (Just e) 1 m') m probs)
  Map.empty . map (Set.toList . imgProblems) . Map.elems . pdImages

repoProblems :: Repository -> NameStats ShortText
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
