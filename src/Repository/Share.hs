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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Sharing the equal exif values of a folder's files and images.
--
-- Each file's exif is read on its own (from exiftool's output, or from
-- a cache), so equal values are separate copies in memory: the same
-- camera, lens, place, keywords or aperture, once for each file (a raw
-- and its JPEG have their own copies of the same capture date, too),
-- and interning the texts ('SymbolizedItem') only shares their
-- characters, not the boxes around them. As most pictures of a folder
-- have the same few values, these copies take much of the
-- repository's memory (about half, on a generated library).
-- 'shareFolder' replaces each value by the first equal one met in the
-- folder, so that each is kept once. The orientation (constants,
-- already shared) and the warnings (rare) are left as they are.
--
-- Values are compared by their exact representation, never by a
-- looser equality: e.g. 'ExifTime''s equality compares instants, so
-- two dates of the same instant in different time zones are equal,
-- but shown differently.
module Repository.Share
  ( shareFolder
  ) where

import           Control.Monad.Trans.State.Strict
import qualified Data.Map.Strict                  as Map
import           Data.Time.LocalTime
import           GHC.Float                        (castDoubleToWord64)

import           Exif
import           Import.NoFoundation              hiding (get)
import           Repository

-- | The values met so far in a folder, by type: each table maps a key
-- that tells values apart exactly to the value kept.
data Tables = Tables
  { tSymbols :: !(Map SymbolizedItem (Maybe SymbolizedItem))
    -- ^ The optional texts (camera, model, place, title...), all
    -- fields together: a 'Just' is shared whatever field it is in.
  , tDoubles :: !(Map Word64 (Maybe Double))
    -- ^ Keyed by their bits, so that e.g. @0.0@ and @-0.0@ stay
    -- apart, and a NaN is a valid key.
  , tIntegers :: !(Map Integer (Maybe Integer))
  , tInts :: !(Map Int (Maybe Int))
  , tSets :: !(Map (Set SymbolizedItem) (Set SymbolizedItem))
    -- ^ The people and the keywords.
  , tLenses :: !(Map LensKey LensInfo)
  , tFlashes :: !(Map (Maybe FlashSource, Maybe SymbolizedItem) FlashInfo)
  , tDates :: !(Map (LocalTime, TimeZone) (Maybe ExifTime))
    -- ^ Keyed by the local time and the time zone, not by the instant.
  }

emptyTables :: Tables
emptyTables = Tables Map.empty Map.empty Map.empty Map.empty Map.empty
                     Map.empty Map.empty Map.empty

type Share = State Tables

-- | Returns the value kept for a key, keeping the given value if the
-- key is new.
keep :: Ord k
     => (Tables -> Map k v)                 -- ^ The table.
     -> (Map k v -> Tables -> Tables)       -- ^ How to store it back.
     -> k -> v -> Share v
keep table store k v = do
  tables <- get
  case Map.lookup k (table tables) of
    Just v' -> return v'
    Nothing -> do
      put $! store (Map.insert k v (table tables)) tables
      return v

-- | 'keep' for an optional value, keyed by the value inside: a 'Just'
-- is kept whole, a 'Nothing' is already shared.
keepJust :: Ord k
         => (Tables -> Map k (Maybe a))
         -> (Map k (Maybe a) -> Tables -> Tables)
         -> (a -> k)                        -- ^ The key of a value.
         -> Maybe a -> Share (Maybe a)
keepJust _     _     _   Nothing    = return Nothing
keepJust table store key m@(Just x) = keep table store (key x) m

shareSymbol :: Maybe SymbolizedItem -> Share (Maybe SymbolizedItem)
shareSymbol = keepJust tSymbols (\t ts -> ts { tSymbols = t }) id

shareDouble :: Maybe Double -> Share (Maybe Double)
shareDouble = keepJust tDoubles (\t ts -> ts { tDoubles = t }) castDoubleToWord64

shareInteger :: Maybe Integer -> Share (Maybe Integer)
shareInteger = keepJust tIntegers (\t ts -> ts { tIntegers = t }) id

shareInt :: Maybe Int -> Share (Maybe Int)
shareInt = keepJust tInts (\t ts -> ts { tInts = t }) id

shareSet :: Set SymbolizedItem -> Share (Set SymbolizedItem)
shareSet s
  | null s    = return s
  | otherwise = keep tSets (\t ts -> ts { tSets = t }) s s

-- | A lens, with its doubles (focal lengths and apertures) as bits:
-- 'LensInfo''s own ordering compares them as numbers, so @0.0@ and
-- @-0.0@ would be one key.
type LensKey = ( SymbolizedItem, SymbolizedItem
               , Maybe (Either Word64 (Word64, Word64))
               , Maybe (Either Word64 (Word64, Word64))
               , Maybe SymbolizedItem )

lensKey :: LensInfo -> LensKey
lensKey (LensInfo name spec fl aperture serial) =
  (name, spec, flKey <$> fl, apKey <$> aperture, serial)
  where flKey (Prime f)              = Left (castDoubleToWord64 f)
        flKey (Zoom a b)             = Right (castDoubleToWord64 a, castDoubleToWord64 b)
        apKey (FixedAperture a)      = Left (castDoubleToWord64 a)
        apKey (VariableAperture a b) = Right (castDoubleToWord64 a, castDoubleToWord64 b)

shareLens :: LensInfo -> Share LensInfo
shareLens l = keep tLenses (\t ts -> ts { tLenses = t }) (lensKey l) l

shareFlash :: FlashInfo -> Share FlashInfo
shareFlash f@(FlashInfo src mode) = keep tFlashes (\t ts -> ts { tFlashes = t }) (src, mode) f

shareDate :: Maybe ExifTime -> Share (Maybe ExifTime)
shareDate = keepJust tDates (\t ts -> ts { tDates = t })
              (\(ExifTime zt) -> (zonedTimeToLocalTime zt, zonedTimeZone zt))

shareExif :: Exif -> Share Exif
shareExif Exif{..} = do
  people       <- shareSet exifPeople
  keywords     <- shareSet exifKeywords
  country      <- shareSymbol exifCountry
  province     <- shareSymbol exifProvince
  city         <- shareSymbol exifCity
  location     <- shareSymbol exifLocation
  camera       <- shareSymbol exifCamera
  model        <- shareSymbol exifModel
  serial       <- shareSymbol exifSerial
  lens         <- shareLens exifLens
  createDate   <- shareDate exifCreateDate
  title        <- shareSymbol exifTitle
  caption      <- shareSymbol exifCaption
  aperture     <- shareDouble exifAperture
  focalLength  <- shareDouble exifFocalLength
  fl35mm       <- shareDouble exifFL35mm
  iso          <- shareInteger exifISO
  sSpeedDesc   <- shareSymbol exifSSpeedDesc
  sSpeedVal    <- shareDouble exifSSpeedVal
  shutterCount <- shareInteger exifShutterCount
  mimeType     <- shareSymbol exifMimeType
  rating       <- shareInt exifRating
  flashInfo    <- shareFlash exifFlashInfo
  width        <- shareInt exifWidth
  height       <- shareInt exifHeight
  megapixels   <- shareDouble exifMegapixels
  make         <- shareSymbol exifMake
  lensMake     <- shareSymbol exifLensMake
  return Exif { exifPeople       = people
              , exifKeywords     = keywords
              , exifCountry      = country
              , exifProvince     = province
              , exifCity         = city
              , exifLocation     = location
              , exifCamera       = camera
              , exifModel        = model
              , exifSerial       = serial
              , exifLens         = lens
              , exifOrientation  = exifOrientation
              , exifCreateDate   = createDate
              , exifTitle        = title
              , exifCaption      = caption
              , exifAperture     = aperture
              , exifFocalLength  = focalLength
              , exifFL35mm       = fl35mm
              , exifISO          = iso
              , exifSSpeedDesc   = sSpeedDesc
              , exifSSpeedVal    = sSpeedVal
              , exifShutterCount = shutterCount
              , exifMimeType     = mimeType
              , exifRating       = rating
              , exifFlashInfo    = flashInfo
              , exifWidth        = width
              , exifHeight       = height
              , exifMegapixels   = megapixels
              , exifMake         = make
              , exifLensMake     = lensMake
              , exifWarning      = exifWarning
              }

shareFile :: File -> Share File
shareFile f = do
  exif <- shareExif (fileExif f)
  return f { fileExif = exif }

shareImage :: Image -> Share Image
shareImage img = do
  raw       <- traverse shareFile (imgRawPath img)
  sidecar   <- traverse shareFile (imgSidecarPath img)
  jpegs     <- mapM shareFile (imgJpegPath img)
  masterMov <- traverse shareFile (imgMasterMov img)
  movs      <- mapM shareFile (imgMovs img)
  untracked <- mapM shareFile (imgUntracked img)
  exif      <- shareExif (imgExif img)
  return img { imgRawPath     = raw
             , imgSidecarPath = sidecar
             , imgJpegPath    = jpegs
             , imgMasterMov   = masterMov
             , imgMovs        = movs
             , imgUntracked   = untracked
             , imgExif        = exif
             }

-- | Shares the equal exif values of a folder's files and images (see
-- the module's description). The result has the same representation
-- as the folder given (the same 'show').
--
-- The tables only live while one folder is shared: a value is kept
-- once per folder, which should be most of the gain (a folder's
-- pictures share their cameras, lenses and places), and leaves the
-- function pure, with nothing kept between folders or scans. The
-- values derived from the images (the time index, the statistics, the
-- group exif) should be computed after it, so that they point to the
-- values kept.
shareFolder :: PicDir -> PicDir
shareFolder dir =
  flip evalState emptyTables $ do
    images <- traverse shareImage (pdImages dir)
    untracked <- mapM shareFile (pdUntracked dir)
    return dir { pdImages = images, pdUntracked = untracked }
