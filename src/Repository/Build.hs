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
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Building images and folders from scanned files, and merging the
-- directories of a folder.
module Repository.Build
  ( addDirToRepo
  , isKnownMediaInode
  , buildFolderFromInodes
  , finishFolder
  , restoreSharing
#ifdef TEST
  , linkComposites
  , linkedStatus
  , linkedExif
  , isBetterMaster
  , selectMasterFile
  , mergePictures
  , mergeFolders
  , splitPathExt
  , mkFileFromInode
  , implicitEventFromDateRange
  , loadImage
#endif
  ) where

import           Control.Applicative
import           Control.DeepSeq
import           Data.List.NonEmpty      (NonEmpty (..))
import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Short         as TS
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           System.FilePath

import           Compat.Orphans          ()
import           Exif
import           Import.NoFoundation     hiding (fileName, fileSize, path)
import           Stats                   (DateRange)

import           Repository
import           Repository.Names

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

-- | Merges two images of the same name, from different files. This
-- is only for images not linked yet (see 'finishFolder').
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
               -- Only part of the role matters here:
               -- - whether the image is a composite image, and its
               --   range, must be carried forward: 'finishFolder'
               --   reads them from the role, and never sets them. It
               --   is one if either file brings a range (both have the
               --   same name, so the same range);
               -- - but a movie never is, even if another file of the
               --   same name (e.g. a poster) brings a range;
               -- - the links (a composite image's components, and
               --   'ComponentOf') don't matter: 'finishFolder'
               --   recomputes them all, so they start empty here.
               , imgRole        = case (ityp, imageRange x <|> imageRange y) of
                                    (MediaMovie, _) -> PlainImage
                                    (_, Just range) -> CompositeOf range []
                                    (_, Nothing)    -> PlainImage
               , imgStatus      = status
               , imgExif        = exif
               }

-- | Merges two directories of the same folder. The values derived
-- from the images are left as they are (empty, for directories just
-- built): 'finishFolder' computes them once all are merged.
mergeFolders :: Config -> PicDir -> PicDir -> PicDir
mergeFolders c x y =
  force $
  x { pdMainPath = pdMainPath bestMainPath
    , pdSecPaths = pdSecPaths x ++ pdMainPath otherMainPath:pdSecPaths y
    , pdImages = newimages
    , pdUntracked = newuntracked
    -- FIXME: record error if we have duplicate event information
    , pdEvent = pickEvent (pdEvent x) (pdEvent y)
    }
  where
    -- The main path is that of the directory with more pictures with
    -- a master, then with more pictures.
    (bestMainPath, otherMainPath) =
      case (compare `on` (\z -> (numMasters z, numPics z))) x y of
                                     GT -> (x, y)
                                     _  -> (y, x)
    numMasters = Map.foldl' (\n img -> if isJust (imgRawPath img) then n + 1 else n)
                            (0 :: Int) . pdImages
    newimages = Map.unionWith (mergePictures c) (pdImages x) (pdImages y)
    newuntracked = pdUntracked x ++ pdUntracked y
    pickEvent a b = explicitEvent a <|> explicitEvent b <|> a <|> b

-- | Keeps only an explicit event (one from a @corydalis.yaml@).
explicitEvent :: Maybe Event -> Maybe Event
explicitEvent mev@(Just ev) =
  case eventSource ev of
    EventExplicit {} -> mev
    EventImplicit {} -> Nothing
explicitEvent Nothing = Nothing

addDirToRepo :: Config -> PicDir -> RepoDirs -> RepoDirs
addDirToRepo config dir =
  Map.insertWith (mergeFolders config) (pdName dir) dir

-- | Strict application of the 'Just' constructor. This is useful as
-- the Maybe type is not strict in its contained value.
strictJust :: a -> Maybe a
strictJust !a = Just a

addImg :: Config -> Map ImageName Image -> Image -> Map ImageName Image
addImg config m i = Map.insertWith (mergePictures config) (imgName i) i m

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

-- | Classifies a scanned inode into an image.
loadImage :: Config
          -> ShortText           -- ^ Folder name.
          -> SymbolizedItem      -- ^ Parent directory path.
          -> Bool                -- ^ Whether the file comes from a source directory.
          -> Map Text EExif      -- ^ Exif cache keyed by inode full name.
          -> InodeInfo           -- ^ Filesystem inode to classify.
          -> Image
loadImage config tname parent isSource lcache ii =
  let rawe = cfgRawExtsSet config
      side = cfgSidecarExts config
      jpeg = cfgJpegExts config
      move = cfgMovieExts config
      ewarn txt = def { exifWarning = Set.singleton txt }
      file_name = inodeFullName ii
      file_text = Text.pack file_name
      (base_full, ext) = splitPathExt file_name
      (base_name, name_range) = parseImageName config base_full
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
      -- A movie is never a composite image: its name is read as for
      -- the other files, but it has no range.
      range = if is_mov then Nothing else name_range
      flags = Flags {
        flagsSoftMaster = isSoftMaster
        }
      mtype = if | is_mov     -> MediaMovie
                 | null untrk -> MediaImage
                 | otherwise  -> MediaUnknown
      untrk = case (raw_file, jpeg_file, sidecar_file, m_mov, p_mov) of
        (Nothing, [], Nothing, Nothing, []) -> [file_obj]
        _                                   -> []
  in force $
       mkImage config (ImageName (TS.fromText base_name_text)) tname
       raw_file sidecar_file jpeg_file m_mov p_mov
       untrk range mtype flags

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
      images = foldl' (\images' f ->
                         addImg config images' (loadImage config tname parent isSource lcache f)
                      ) Map.empty known
      untracked = map (\ii -> mkFileFromInode parent ii def) otherFiles
  -- The derived values are left empty: 'finishFolder' computes them,
  -- once the folder's directories are merged.
  in PicDir { pdName = tname
            , pdMainPath = dirpath
            , pdSecPaths = []
            , pdImages = images
            , pdTimeSort = Set.empty
            , pdUntracked = untracked
            , pdYear = Nothing
            , pdTimestamp = Nothing
            , pdExif = def
            , pdStats = zeroStats
            , pdEvent = yamlEvent }

-- | Links the composite images of a folder to their components, by
-- name: a composite image (an image with a range) lists the images of
-- its range that exist, in order, and each of them lists the composite
-- images covering it. A composite image is never linked as a
-- component (with the sample regexes, a name can't be both, but the
-- regexes can be changed), and neither is a movie, as movies are not
-- part of composite images.
linkComposites :: Map ImageName Image -> Map ImageName Image
linkComposites images =
  Map.mapWithKey (\name img -> img { imgRole = role name img }) images
  where
    -- Each composite image, with the names of its range that are
    -- components (a map of names to lists of names). For each image,
    -- 'imageRange' gives Nothing if it is not a composite image, and
    -- 'Map.mapMaybe' drops it; otherwise, inside the Just ('fmap'),
    -- the names of the range are listed and only the components kept.
    -- E.g. @d_01-03@ gives @[d_01, d_03]@ when @d_02@ is missing.
    composites = Map.mapMaybe (fmap (filter canBeComponent . rangeNames) . imageRange) images
    -- Whether a name can be a component: an image of the folder that
    -- is neither a composite image itself nor a movie.
    canBeComponent name =
      maybe False (\img -> isNothing (imageRange img) && imgType img /= MediaMovie)
            (Map.lookup name images)
    -- The reverse of 'composites': each component, with the composite
    -- images covering it, in the order of their names. It is built
    -- from all the (component, composite) pairs, merging the lists of
    -- the same component. 'Map.fromListWith' calls its function as
    -- @f new old@: going through the composite images in descending
    -- order, and putting the new one first, gives ascending lists, at
    -- a constant cost per pair (as '<>' walks only its left side).
    coveredBy = Map.fromListWith (<>)
                  [ (component, composite :| [])
                  | (composite, components) <- Map.toDescList composites
                  , component <- components ]
    -- A composite image keeps its range, and gets its components from
    -- 'composites'. That lookup can't fail, as 'composites' has an
    -- entry for every image with a range, but the types can't say so:
    -- the default is never used.
    role name img =
      case imgRole img of
        CompositeOf range _ -> CompositeOf range (Map.findWithDefault [] name composites)
        _                   -> maybe PlainImage ComponentOf (Map.lookup name coveredBy)

-- | The status of an image once its folder's links are known. A
-- component with a master and no output of its own (unprocessed)
-- counts as processed, as soon as a composite image covers it (even
-- one without an output yet). A composite image with an output and no
-- master (standalone) counts as processed when any of its components
-- has a master. Other images keep the status their files give.
linkedStatus :: Map ImageName Image -> Image -> ImageStatus
linkedStatus images img =
  case (imgRole img, imgStatus img) of
    (ComponentOf _, ImageUnprocessed) -> ImageProcessed
    (CompositeOf _ components, ImageStandalone)
      | any hasMaster components -> ImageProcessed
    (_, status) -> status
  where hasMaster name = maybe False (isJust . imgRawPath) (Map.lookup name images)

-- | The earliest date among images, if any has one.
earliestDate :: Map ImageName Image -> Maybe LocalTime
earliestDate =
  -- Evaluated at each step (not only down to its Just).
  Map.foldl' (\a img ->
                case (a, imageLocalDate img) of
                  (Just x, Just y) -> Just $! min x y
                  (Nothing, d)     -> d
                  (_, Nothing)     -> a
             ) Nothing

-- | Fills in a composite image's exif from its first component's (see
-- 'fillCompositeExif'). Other images are left as they are.
linkedExif :: Map ImageName Image -> Image -> Exif
linkedExif images img =
  case imgRole img of
    CompositeOf _ (name:_) | Just component <- Map.lookup name images ->
      fillCompositeExif (imgExif component) (imgExif img)
    _ -> imgExif img

-- | Restores, in a folder read from the repository cache, the sharing
-- between its values that a scan builds, and that reading the cache
-- loses, as each value is decoded on its own, as a copy. Without it, a
-- repository read from the cache takes more memory than the one the
-- scan built, e.g. an image's exif is then a copy of what it takes
-- from its files' exif, and the time index holds copies of the images'
-- names and of the parts of their dates.
--
-- Each image's exif is computed again from its files' exif, as a scan
-- does ('promoteFileExif', as in 'mkImage' and 'mergePictures'), the
-- images are keyed by their own names, and the folder is finished
-- again ('finishFolder'), which builds the links, the composite
-- images' exif, and the values derived from the images, from the
-- images. Finishing a folder a second time gives the same folder, so
-- the result is equal to the folder read.
restoreSharing :: PicDir -> PicDir
restoreSharing dir =
  let fromFiles img =
        img { imgExif = promoteFileExif (fileExif <$> imgRawPath img)
                                        (fileExif <$> imgSidecarPath img)
                                        (map fileExif (imgJpegPath img))
                                        (fileExif <$> imgMasterMov img)
                                        (map fileExif (imgMovs img)) }
      images = Map.fromDistinctAscList
                 [ (imgName img, fromFiles img) | img <- Map.elems (pdImages dir) ]
  in finishFolder dir { pdImages = images }

-- | Finishes a folder, once all its directories are merged: links the
-- composite images and their components, sets the statuses and the
-- composite images' exif that depend on the links, and computes the
-- values derived from the images, once, from the final images. Until
-- then, these values are empty (see 'buildFolderFromInodes' and
-- 'mergeFolders').
finishFolder :: PicDir -> PicDir
finishFolder dir =
  let linked = linkComposites (pdImages dir)
      images = Map.map (\img -> img { imgStatus = linkedStatus linked img
                                    , imgExif = linkedExif linked img
                                    }) linked
      stats = addUntrackedFiles (pdUntracked dir) (computeImagesStats images)
      timestamp = earliestDate images
  in dir { pdImages = images
         , pdTimeSort = buildTimeSort images
         , pdYear = localDateToYear <$> timestamp
         , pdTimestamp = timestamp
         , pdExif = buildGroupExif images
         , pdStats = stats
         -- An implicit event is computed again, so that finishing a
         -- folder twice gives the same result.
         , pdEvent = explicitEvent (pdEvent dir) <|>
                     implicitEventFromDateRange (pdName dir) (sDateRange stats)
         }
