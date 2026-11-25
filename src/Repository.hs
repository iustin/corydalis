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

{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | The repository of pictures: the types it is made of (files,
-- images, folders, statistics), and the pure functions on them. It
-- also defines what the repository cache holds ('CachedRepo'), and
-- pins its format version ('repoCacheVersion'; see
-- "Repository.Encoding").
module Repository
  ( MimeType
  , File(..)
  , fileFullPath
  , fileRelPath
  , fileMimeType
  , Flags(..)
  , emptyFlags
  , MediaType(..)
  , ImageRange(..)
  , ImageRole(..)
  , Image(..)
  , InodeInfo(..)
  , inodeFullName
  , localDateToYear
  , imageYear
  , imageYearMonth
  , imageLocalDate
  , ImageTimeKey
  , imageTimeKey
  , mkImageStatus
  , mkImage
  , PicDir(..)
  , RepoDirs
  , RepoStatus(..)
  , Repository(..)
  , startupRepository
  , rescanningRepository
  , Stats(..)
  , RepoStats(..)
  , SearchResultsPics
  , SearchResults
  , zeroStats
  , totalStatsSize
  , totalStatsCount
  , sumStats
  , updateStatsWithPic
  , computeImagesStats
  , addUntrackedFiles
  , computeFolderStats
  , StrictPair(..)
  , StrictTriple(..)
  , computeRepoStats
  , repoGlobalExif
  , ImageNeighbours(..)
  , imageNeighbours
  , numRawPics
  , numPics
  , numUnprocessedPics
  , numProcessedPics
  , numStandalonePics
  , hasViewablePics
  , folderClass
  , folderClassFromStats
  , buildGroupExif
  , buildTimeSort
  , filterImagesByClass
  , filterImagesBy
  , countImagesBy
  , imageDateRange
  , filterFoldersBy
  , imageHasImages
  , imageHasMovies
  , imageHasUntracked
  , allImageFiles
  , allViewableImageFiles
  , allRepoFiles
  , imgProblems
  , pdProblems
  , repoProblems
  , CacheRevision(..)
  , CachedRepo
  , repoCacheVersion
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.Data               (Data)
import           Data.List.NonEmpty      (NonEmpty)
import qualified Data.Foldable
import qualified Data.Map.Strict         as Map
import           Data.Semigroup
import qualified Data.Set                as Set
import           Data.Store              (Store)
import           Data.Store.Version      (VersionConfig, hashedVersionConfig)
import           Data.Char               (isAlphaNum, isAscii)
import qualified Data.Text               as Text
import qualified Data.Text.Short         as TS
import           Data.Time.Calendar
import           Data.Time.Clock.POSIX
import           Data.Time.LocalTime
import           System.FilePath
import           System.Posix.Types

import           AtomTypes               (EventKindOp, extractEventType)
import           Compat.Orphans          ()
import           Exif
import           Import.NoFoundation     hiding (fileName, fileSize, path)
import           Stats                   (CameraInfo (..), DateRange,
                                          Occurrence (..), mergeMinMaxPair,
                                          ocFromSize)

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
  } deriving (Show, Eq, Generic, Data)

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
  } deriving (Show, Eq, Generic, Data)

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
               deriving (Show, Eq, Ord, Generic, Data)

instance Store MediaType

instance NFData MediaType where
  rnf = rwhnf

-- | The range in a composite image's name, as the range regex reads
-- it: @d_01-03@ has the root @d_@, the numbers 1 and 3, padded to two
-- digits. The root includes the directory part of the name, for a
-- picture in a sub-directory of its folder.
--
-- The last number is below the first for a camera counter that
-- wrapped (both numbers of four digits): @dsc_9998-0002@ runs up to
-- 9999, then from 1 (see 'Repository.Names.rangeNames').
data ImageRange = ImageRange
  { irRoot  :: !ShortText
  , irFirst :: !Int
  , irLast  :: !Int
  , irWidth :: !Int  -- ^ The width the numbers are padded to.
  } deriving (Show, Eq, Generic, Data)

instance Store ImageRange

instance NFData ImageRange where
  rnf = rwhnf

-- | How an image relates to composite images, as linked by name in
-- its folder: a composite image lists its components, and a component
-- lists the composite images that cover it. An image is never both: a
-- composite image is never linked as another one's component.
data ImageRole
  = PlainImage
    -- ^ Neither a composite image nor a component.
  | CompositeOf ![ImageName]
    -- ^ A composite image (an image with a range), with the components
    -- that exist in its folder, in the order of its range (maybe none).
  | ComponentOf !(NonEmpty ImageName)
    -- ^ A component of the composite images listed.
  deriving (Show, Eq, Generic, Data)

instance Store ImageRole

instance NFData ImageRole where
  rnf PlainImage       = ()
  rnf (CompositeOf ns) = rnf ns
  rnf (ComponentOf ns) = rnf ns

data Image = Image
    { imgName        :: !ImageName
    , imgParent      :: !ShortText
    , imgRawPath     :: !(Maybe File)
    , imgSidecarPath :: !(Maybe File)
    , imgJpegPath    :: ![File]
    , imgMasterMov   :: !(Maybe File)
    , imgMovs        :: ![File]
    , imgUntracked   :: ![File]
    , imgRange       :: !(Maybe ImageRange)
    , imgRole        :: !ImageRole  -- ^ Only set once its folder is
                                    -- finished ('PlainImage' before);
                                    -- then 'CompositeOf' exactly for
                                    -- the images with a range.
    , imgExif        :: !Exif
    , imgType        :: !MediaType
    , imgStatus      :: !ImageStatus
    , imgFlags       :: !Flags
    } deriving (Show, Eq, Generic, Data)

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
                  rnf imgRole        `seq`
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
        -> Maybe ImageRange   -- ^ Range
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
  in Image name parent raw sidecar jpegs mmov movs untrk range PlainImage exif mtype status

-- | A picture folder. The values derived from its images (the time
-- index, year, first date, group exif, stats, and the implicit event)
-- are computed once all its directories are merged, by
-- 'Repository.Build.finishFolder'; until then they are empty, and the
-- event is only the one from the folder's @corydalis.yaml@, if any.
-- The images' roles, and the statuses and exif that depend on them,
-- are also only final then.
data PicDir = PicDir
  { pdName      :: !ShortText
  , pdMainPath  :: !ShortText
  , pdSecPaths  :: ![ShortText]
  , pdImages    :: !(Map ImageName Image)
  , pdTimeSort  :: !(Set ImageTimeKey)
  , pdUntracked :: ![File]           -- ^ Files of unknown type, not treated as images.
  , pdYear      :: !(Maybe Integer)  -- ^ The approximate year of the
                                     -- earliest picture.
  , pdTimestamp :: !(Maybe LocalTime)  -- ^ The approximate date of the
                                       -- first picture.
  , pdExif      :: !GroupExif
  , pdStats     :: !Stats
  , pdEvent     :: !(Maybe Event)
  } deriving (Show, Generic, Data)

instance Store PicDir

instance NFData PicDir where
  rnf PicDir{..} = rnf pdName      `seq`
                   rnf pdMainPath  `seq`
                   rnf pdSecPaths  `seq`
                   rnf pdImages    `seq`
                   rnf pdTimeSort  `seq`
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
                | RepoStarting -- ^ Denotes a repository about to be
                               -- rescanned; it still holds the
                               -- previous data (or none, at startup).
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
  deriving (Show, Generic, Data)

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
  } deriving (Show, Generic, Data)

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
  } deriving (Show, Generic, Data)

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
  } deriving (Show, Generic, Data)

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

-- | A pair whose parts are evaluated when it is built. For the
-- accumulator of a strict fold: with a plain pair, only the pair is
-- evaluated at each step, and its parts become chains of pending
-- updates, as long as what is folded over.
data StrictPair a b = StrictPair !a !b

-- | A triple whose parts are evaluated when it is built (see
-- 'StrictPair').
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

buildGroupExif :: Map ImageName Image -> GroupExif
buildGroupExif =
  Map.foldl' (\e img -> addExifToGroup e (imgExif img)) def

buildTimeSort :: Map ImageName Image -> Set ImageTimeKey
buildTimeSort = Set.fromList . map imageTimeKey . Map.elems

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

-- | A revision marker for the repository cache. 'Store' writes nothing
-- for it, but the name of its constructor is part of the shape that
-- 'repoCacheVersion' hashes. Rename the constructor (to the next
-- number) when an encoding changes in a way the shape doesn't show,
-- so that older caches are ignored: a change to a hand-written 'Store'
-- instance (that of 'ShortText' in "Compat.Orphans", or those of
-- 'Regex' and 'SymbolizedItem'), or to a library's encoding of a type
-- whose name stays the same.
data CacheRevision = CacheRevision1
  deriving (Show, Eq, Generic, Data)

instance Store CacheRevision

-- | What the repository cache holds: the revision marker, the
-- configuration the repository was built with, and the repository.
type CachedRepo = (CacheRevision, Config, Repository)

-- | The expected format version of the repository cache, which
-- "Repository.Encoding" uses. The version written to the cache is a
-- hash of the shape of 'CachedRepo' (the names of its types,
-- constructors and fields, and the types of the fields), computed when
-- compiling; the hash here is only checked against it, and the build
-- fails when they differ. Then check that the change is the intended
-- one, and copy the computed hash here: in the build error, it is the
-- one called \"expected\", the one called \"specified\" being this one.
-- It is defined here, not in "Repository.Encoding", because a value
-- used at compile time can't come from the module that uses it.
repoCacheVersion :: VersionConfig CachedRepo
repoCacheVersion = hashedVersionConfig "nG_riv0nGzXQbYzz6UqtMDraQgM="
