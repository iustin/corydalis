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
  , mergeShadows
  , resolveProcessedRanges
#ifdef TEST
  , isBetterMaster
  , selectMasterFile
  , mergePictures
  , mergeFolders
  , splitPathExt
  , mkFileFromInode
  , implicitEventFromDateRange
  , loadImage
  , maybeUpdateStandaloneRange
#endif
  ) where

import           Control.Applicative
import           Control.DeepSeq
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

addDirToRepo :: Config -> PicDir -> RepoDirs -> RepoDirs
addDirToRepo config dir =
  Map.insertWith (mergeFolders config) (pdName dir) dir

-- | Strict application of the 'Just' constructor. This is useful as
-- the Maybe type is not strict in its contained value.
strictJust :: a -> Maybe a
strictJust !a = Just a

addImgs :: Config -> Map ImageName Image -> [Image] -> Map ImageName Image
addImgs config = foldl' (addImg config)

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
      snames = expandRangeFile config base_name
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
      -- A strict pair: with a lazy one, the maps would only be built
      -- once the fold is done, from a chain of updates holding every
      -- image still to be loaded.
      StrictPair images shadows =
        foldl' (\(StrictPair images' shadows') f ->
                  let (img, newss) = loadImage config tname parent isSource lcache f
                  in StrictPair (addImg config images' img) (addImgs config shadows' newss)
               ) (StrictPair Map.empty Map.empty) known
      untracked = map (\ii -> mkFileFromInode parent ii def) otherFiles
      -- The earliest date, evaluated at each step (not only down to
      -- its Just).
      timestamp = Map.foldl' (\a img ->
                                case (a, imageLocalDate img) of
                                  (Just x, Just y) -> Just $! min x y
                                  (Nothing, d)     -> d
                                  (_, Nothing)     -> a
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

mergeShadows :: Config -> PicDir -> PicDir
mergeShadows config picd =
  let images' =
        Map.foldlWithKey' (\imgs sname shadow ->
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
        Map.foldl' (\accimgs img ->
                      case maybeUpdateStandaloneRange  config picd img of
                        Nothing   -> accimgs
                        Just img' -> Map.insert (imgName img') img' accimgs)
             (pdImages picd) (pdImages picd)
  in picd { pdImages = images' }
