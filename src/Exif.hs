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

{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TupleSections      #-}

module Exif ( Exif(..)
            , GroupExif(..)
            , Rotate(..)
            , Transform(..)
            , LensInfo(..)
            , LensFocalLength(..)
            , LensAperture(..)
            , LensType(..)
            , ExifTime(..)
            , FlashSource(..)
            , FlashInfo(..)
            , NameStats
            , EExif
            , getExif
            , exifLocalCreateDate
            , promoteFileExif
            , addExifToGroup
            , affineTransform
            , unknown
            , unknownLens
            , lensDisplayName
            , lensShortName
            , lensType
            , formatPerson
            , rotateToJSON
            , transformParams
            , transformMatrix
            ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Monad             (fail, msum)
import           Control.Monad.Trans.State
import           Data.Aeson
import           Data.Aeson.Types          (Parser, modifyFailure, parseEither,
                                            parseMaybe, typeMismatch)
import           Data.Bifunctor
import qualified Data.ByteString           as BS (ByteString, readFile)
import           Data.Default
import qualified Data.Map.Strict           as Map
import           Data.Scientific           (toBoundedInteger)
import           Data.Semigroup
import qualified Data.Set                  as Set
import           Data.Store
import           Data.Store.TH             (makeStore)
import qualified Data.Text                 as Text
import qualified Data.Text.Read            as Text
import           Data.Time.Format
import           Data.Time.LocalTime
import           System.Process.Typed

import           Cache
import           Compat.Orphans            ()
import           Import.NoFoundation       hiding (get)

-- | Shutter counts this high are unlikely, but they do appear in
-- corrupted/wrong exif data.
tooHighShutterCount :: Integer
tooHighShutterCount = 10_000_000

data Orientation
  = OrientationTopLeft
  | OrientationTopRight
  | OrientationBotRight
  | OrientationBotLeft
  | OrientationLeftTop
  | OrientationRightTop
  | OrientationRightBot
  | OrientationLeftBot
  deriving (Show, Eq)

instance Default Orientation where
  def = OrientationTopLeft

instance FromJSON Orientation where
  parseJSON = withScientific "ExifOrientation" $ \n ->
    case toBoundedInteger n::Maybe Int of
      Just 1  -> return OrientationTopLeft
      Just 2  -> return OrientationTopRight
      Just 3  -> return OrientationBotRight
      Just 4  -> return OrientationBotLeft
      Just 5  -> return OrientationLeftTop
      Just 6  -> return OrientationRightTop
      Just 7  -> return OrientationRightBot
      Just 8  -> return OrientationLeftBot
      Just v  ->
        fail $ formatToString ("Invalid orientation value '" % int % "'") v
      Nothing ->
        fail $ formatToString ("Non-integer orientation value '" % shown % "'") n

$(makeStore ''Orientation)

extractRaw :: (FromJSON a) => Text -> Value -> Parser a
extractRaw t =
  modifyFailure (\s -> "Parsing " ++ desc ++ ": " ++ s) .
  withObject (desc ++ "/num|val") (\o -> o .: "num" <|> o .: "val")
  where desc = Text.unpack t

extractParsed :: (FromJSON a) => Text -> Value -> Parser a
extractParsed t =
  modifyFailure (\s -> "Parsing " ++ desc ++ ": " ++ s) .
  withObject (desc ++ "/val") (.: "val")
  where desc = Text.unpack t

rawValue :: (FromJSON a) => Object -> Text -> Parser a
rawValue parent key =
  parent .: key >>= extractRaw key

(.!:) :: (FromJSON a) => Object -> Text -> Parser a
(.!:) = rawValue

parsedValue :: (FromJSON a) => Object -> Text -> Parser a
parsedValue parent key =
  parent .: key >>= extractParsed key

(.~:) :: (FromJSON a) => Object -> Text -> Parser a
(.~:) = parsedValue

optRawValue :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
optRawValue parent key = do
  e <- parent .:? key
  case e of
    Nothing -> return Nothing
    Just e' -> Just <$> extractRaw key e'

(.!:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
(.!:?) = optRawValue

optParsedValue :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
optParsedValue parent key = do
  e <- parent .:? key
  case e of
    Nothing -> return Nothing
    Just e' -> Just <$> extractParsed key e'

(.~:?) :: (FromJSON a) => Object -> Text -> Parser (Maybe a)
(.~:?) = optParsedValue

data LensFocalLength
  = Prime !Double
  | Zoom !Double !Double
  deriving (Show, Eq, Ord)

$(makeStore ''LensFocalLength)

instance NFData LensFocalLength where
  rnf (Prime x)  = rnf x
  rnf (Zoom x y) = rnf x `seq` rnf y

data LensAperture
  = FixedAperture !Double
  | VariableAperture !Double !Double
  deriving (Show, Eq, Ord)

$(makeStore ''LensAperture)

instance NFData LensAperture where
  rnf (FixedAperture x)      = rnf x
  rnf (VariableAperture x y) = rnf x `seq` rnf y

data LensInfo = LensInfo
  { liName   :: !Text
  , liSpec   :: !Text
  , liFL     :: !(Maybe LensFocalLength)
  , liAp     :: !(Maybe LensAperture)
  , liSerial :: !(Maybe Text)
  } deriving (Show, Eq, Ord)

-- FIXME: should we error out instead?
instance Semigroup LensInfo where
  x <> _ = x

$(makeStore ''LensInfo)

instance NFData LensInfo where
  rnf LensInfo{..} = rnf liName `seq`
                     rnf liSpec `seq`
                     rnf liFL   `seq`
                     rnf liAp   `seq`
                     rnf liSerial

instance Default LensInfo where
  def = unknownLens

unknownLens :: LensInfo
unknownLens = LensInfo unknown unknown Nothing Nothing Nothing

data LensType = LensPrime
              | LensConstantApertureZoom
              | LensVariableApertureZoom
              | LensUnknown

lensType :: LensInfo -> LensType
lensType LensInfo{..} =
  case (liFL, liAp) of
    (Nothing, _)                             -> LensUnknown
    (_, Nothing)                             -> LensUnknown
    (Just Prime {}, _)                       -> LensPrime
    (Just Zoom {}, Just FixedAperture {})    -> LensConstantApertureZoom
    (Just Zoom {}, Just VariableAperture {}) -> LensVariableApertureZoom

lensInfoFromObject :: Object -> Parser LensInfo
lensInfoFromObject o = do
  liSpec <- o .~: "LensSpec"  <|>
            o .~: "LensInfo"  <|>
            o .~: "LensModel" <|>
            o .~: "Lens"      <|>
            pure unknown
  liName <- o .~: "LensID"    <|>
            o .~: "LensModel" <|>
            pure liSpec
  minFL <- o .!:? "MinFocalLength"
  maxFL <- o .!:? "MaxFocalLength"
  let liFL =
        case (minFL, maxFL) of
          (Just m1, Just m2) -> if m1 == m2
                                  then Just $ Prime m1
                                  else Just $ Zoom m1 m2
          _ -> Nothing
  apMinFL <- o .!:? "MaxApertureAtMinFocal"
  apMaxFL <- o .!:? "MaxApertureAtMaxFocal"
  let liAp =
        case (apMinFL, apMaxFL) of
          (Just a1, Just a2) -> if a1 == a2
                                  then Just $ FixedAperture a1
                                  else Just $ VariableAperture a1 a2
          _ -> Nothing
  serial <- o .~:? "LensSerialNumber"
  -- TODO: merge this with camera serial parsing
  liSerial <- case serial of
                Nothing -> pure Nothing
                -- Theory: if the camera supports this fiels, but the
                -- lens doesn't report it (to the body), it gets
                -- recorded as empty string.
                Just "" -> pure Nothing
                Just v  -> Just <$> parseStrOrNum v
  return LensInfo{..}

lensDisplayName :: LensInfo -> Text
lensDisplayName LensInfo{..} =
  if "Unknown (" `Text.isPrefixOf` liName &&
     liSpec /= unknown
    then liSpec
    else liName

lensShortName :: LensInfo -> Text
lensShortName LensInfo{..} =
  let name = if Text.length liSpec < Text.length liName
        then liSpec
        else liName
  -- TODO: merge with camera name
  in maybe name (\s -> Text.concat [name, " (#", s, ")"]) liSerial

parseStrOrNum :: Value -> Parser Text
parseStrOrNum (String f) = pure f
parseStrOrNum (Number n) =
  -- try to parse the number as int as much as possible
  maybe (showfn n) showfn (toBoundedInteger n::Maybe Int)
  where
    showfn :: (Show a) => a -> Parser Text
    showfn = pure . sformat shown
parseStrOrNum v          = typeMismatch "string or number" v

-- | Time type used in (our) exif structures.
--
-- It's just an alias to ZonedTime, but with Eq (and thus Ord)
-- instances.
newtype ExifTime = ExifTime { etTime :: ZonedTime }
  deriving (Show)

instance NFData ExifTime where
  rnf = rnf . etTime

-- | Custom Eq instance that checks UTC time equality.
--
-- It could be that this fails in some cases - one versio of an image
-- has correct TZ information, another not, but at least will be
-- self-consistent.
instance Eq ExifTime where
  ExifTime a == ExifTime b =
    -- TODO: check on time + zone instead?
    zonedTimeToUTC a == zonedTimeToUTC b

-- | Custom Ord instance that orders on UTC time.
--
-- See comment on Eq instance.
instance Ord ExifTime where
  ExifTime a `compare` ExifTime b =
    zonedTimeToUTC a `compare` zonedTimeToUTC b

data FlashSource
  = FlashSourceNone
  | FlashSourceInternal
  | FlashSourceExternal
  deriving (Eq, Ord, Show)

instance NFData FlashSource where
  rnf = rwhnf

parseFlashSource :: (Integral a) => a -> Maybe FlashSource
parseFlashSource 0 = Just FlashSourceNone
parseFlashSource 1 = Just FlashSourceExternal
parseFlashSource 2 = Just FlashSourceInternal
parseFlashSource _ = Nothing

data FlashInfo = FlashInfo
  { fiSource :: !(Maybe FlashSource)
  , fiMode   :: !(Maybe Text)
  } deriving (Eq, Show)

instance Default FlashInfo where
  def = FlashInfo Nothing Nothing

instance NFData FlashInfo where
  rnf FlashInfo{..} = rnf fiSource `seq`
                      rnf fiMode

data RawExif = RawExif
  { rExifSrcFile      :: Text
  , rExifModel        :: Maybe Text
  , rExifSerial       :: Maybe Text
  , rExifLens         :: Maybe LensInfo
  , rExifOrientation  :: Maybe Orientation
  , rExifHSubjects    :: [[Text]]
  , rExifPeople       :: [Text]
  , rExifCountry      :: Maybe Text
  , rExifProvince     :: Maybe Text
  , rExifCity         :: Maybe Text
  , rExifLocation     :: Maybe Text
  , rExifCreateDate   :: Maybe ExifTime
  , rExifTitle        :: Maybe Text
  , rExifCaption      :: Maybe Text
  , rExifAperture     :: Maybe Double
  , rExifFocalLength  :: Maybe Double
  , rExifFL35mm       :: Maybe Double
  , rExifISO          :: Maybe Integer
  , rExifSSpeedDesc   :: Maybe Text
  , rExifSSpeedVal    :: Maybe Double
  , rExifShutterCount :: Maybe Integer
  , rExifMimeType     :: Maybe Text
  , rExifRating       :: Maybe Int
  , rExifFlashSource  :: Maybe Int
  , rExifFlashMode    :: Maybe Text
  -- met fields below
  , rExifRaw          :: Object
  , rExifWarning      :: Maybe Text
  } deriving (Show)

instance FromJSON RawExif where
  parseJSON = withObject "RawExif" $ \o -> do
    let giveUp        = pure Nothing
    rExifSrcFile     <- o .: "SourceFile"
    rExifModel       <- o .!:? "Model"
    cameraSerial     <- o .~:? "SerialNumber"
    rExifSerial      <- case cameraSerial of
                          Nothing -> pure Nothing
                          Just v  -> Just <$> parseStrOrNum v
    rExifLens        <- (Just <$> lensInfoFromObject o) <|>
                        giveUp
    rExifOrientation <- o .!:? "Orientation"
    hsubjs           <- o .!:? "HierarchicalSubject" .!= []
    let rExifHSubjects = map parseHierSubject hsubjs
    rExifPeople      <- o .!:? "PersonInImage" .!= []
    rExifCountry     <- o .~: "Country" <|>
                        o .~: "Country-PrimaryLocationName" <|>
                        giveUp
    rExifProvince    <- o .~: "Province-State" <|>
                        o .~: "State" <|>
                        giveUp
    rExifCity        <- o .~:? "City"
    rExifLocation    <- o .~: "Sub-location" <|>
                        o .~: "Location" <|>
                        giveUp
    rExifCreateDate  <- parseCreateDate o <|> giveUp
    rExifTitle       <- o .!:? "Title"
    rExifCaption     <- o .!:? "Caption-Abstract"
    rExifAperture    <- o .!:? "Aperture"
    rExifFocalLength <- o .!:? "FocalLength"
    rExifFL35mm      <- o .!: "FocalLengthIn35mmFormat" <|>
                        o .!: "FocalLength35efl" <|>
                        giveUp
    isoval           <- o .~:? "ISO"
    rExifISO         <- case isoval of
                          Nothing -> pure Nothing
                          Just v  -> Just <$> parseValOrList v
    rssd             <- o .~:? "ShutterSpeed"
    rExifSSpeedDesc  <- case rssd of
                          Nothing -> pure Nothing
                          Just v  -> Just <$> parseStrOrNum v
    rExifSSpeedVal   <- o .!:? "ShutterSpeed"
    rExifShutterCount <- o .~:? "ShutterCount"
    rExifMimeType    <- o .~:? "MIMEType"
    rExifRating      <- o .~:? "Rating"
    rExifFlashSource <- o .!:? "FlashSource"
    rExifFlashMode   <- o .~:? "FlashMode"
    -- meta fields below
    rExifWarning     <- o .~:? "Warning"
    let rExifRaw      = o
    return RawExif{..}

-- TODO: Replace exifRating Int with a proper rating type? [question]

data Exif = Exif
  { exifPeople       :: !(Set Text)
  , exifKeywords     :: !(Set Text)
  , exifCountry      :: !(Maybe Text)
  , exifProvince     :: !(Maybe Text)
  , exifCity         :: !(Maybe Text)
  , exifLocation     :: !(Maybe Text)
  , exifCamera       :: !(Maybe Text)
  , exifModel        :: !(Maybe Text)
  , exifSerial       :: !(Maybe Text)
  , exifLens         :: !LensInfo
  , exifOrientation  :: !Orientation
  , exifCreateDate   :: !(Maybe ExifTime)
  , exifTitle        :: !(Maybe Text)
  , exifCaption      :: !(Maybe Text)
  , exifAperture     :: !(Maybe Double)
  , exifFocalLength  :: !(Maybe Double)
  , exifFL35mm       :: !(Maybe Double)
  , exifISO          :: !(Maybe Integer)
  , exifSSpeedDesc   :: !(Maybe Text)
  , exifSSpeedVal    :: !(Maybe Double)
  , exifShutterCount :: !(Maybe Integer)
  , exifMimeType     :: !(Maybe Text)
  , exifRating       :: !(Maybe Int)
  , exifFlashInfo    :: !FlashInfo
  -- meta field
  , exifWarning      :: !(Set Text)
  } deriving (Show, Eq)

instance NFData Exif where
  rnf Exif{..} = rnf exifPeople       `seq`
                 rnf exifKeywords     `seq`
                 rnf exifCountry      `seq`
                 rnf exifProvince     `seq`
                 rnf exifCity         `seq`
                 rnf exifLocation     `seq`
                 rnf exifCamera       `seq`
                 rnf exifModel        `seq`
                 rnf exifSerial       `seq`
                 rnf exifLens         `seq`
                 rnf exifCreateDate   `seq`
                 rnf exifTitle        `seq`
                 rnf exifCaption      `seq`
                 rnf exifAperture     `seq`
                 rnf exifFocalLength  `seq`
                 rnf exifFL35mm       `seq`
                 rnf exifISO          `seq`
                 rnf exifSSpeedDesc   `seq`
                 rnf exifSSpeedVal    `seq`
                 rnf exifShutterCount `seq`
                 rnf exifMimeType     `seq`
                 rnf exifRating       `seq`
                 rnf exifFlashInfo    `seq`
                 rnf exifWarning

instance Default Exif where
  def = Exif { exifPeople       = Set.empty
             , exifKeywords     = Set.empty
             , exifCountry      = Nothing
             , exifProvince     = Nothing
             , exifCity         = Nothing
             , exifLocation     = Nothing
             , exifCamera       = Nothing
             , exifModel        = Nothing
             , exifSerial       = Nothing
             , exifLens         = def
             , exifOrientation  = def
             , exifCreateDate   = Nothing
             , exifTitle        = Nothing
             , exifCaption      = Nothing
             , exifAperture     = Nothing
             , exifFocalLength  = Nothing
             , exifFL35mm       = Nothing
             , exifISO          = Nothing
             , exifSSpeedDesc   = Nothing
             , exifSSpeedVal    = Nothing
             , exifShutterCount = Nothing
             , exifMimeType     = Nothing
             , exifRating       = Nothing
             , exifFlashInfo    = def
             , exifWarning      = Set.empty
             }

-- | Type alias for either an error or the exif data (which, of
-- course, might be fully "empty" itself).
type EExif = Either Text Exif

-- | Data type for raw exif parse failures.
data FailRExif = FailRExif
  { freFilePath :: FilePath
  , freMessage  :: Text
  , freValue    :: Maybe Value
  }

-- | Type alias for either an error in parsing the raw exif data, or
-- the actual data.
type ERawExif = Either FailRExif RawExif

type NameStats a = Map (Maybe a) Integer

data GroupExif = GroupExif
  { gExifPeople    :: !(NameStats Text)
  , gExifKeywords  :: !(NameStats Text)
  , gExifCountries :: !(NameStats Text)
  , gExifProvinces :: !(NameStats Text)
  , gExifCities    :: !(NameStats Text)
  , gExifLocations :: !(NameStats Text)
  , gExifCameras   :: !(NameStats Text)
  , gExifLenses    :: !(NameStats Text)
  , gExifTitles    :: !(NameStats Text)
  , gExifCaptions  :: !(NameStats Text)
  , gExifPeopleCnt :: !(NameStats Int)
  , gExifKwdCnt    :: !(NameStats Int)
  , gExifFlashSrc  :: !(NameStats FlashSource)
  -- TODO: add warnings?
  } deriving (Show)

instance NFData GroupExif where
  rnf GroupExif{..} = rnf gExifPeople    `seq`
                      rnf gExifKeywords  `seq`
                      rnf gExifCountries `seq`
                      rnf gExifProvinces `seq`
                      rnf gExifCities    `seq`
                      rnf gExifLocations `seq`
                      rnf gExifCameras   `seq`
                      rnf gExifLenses    `seq`
                      rnf gExifTitles    `seq`
                      rnf gExifCaptions  `seq`
                      rnf gExifPeopleCnt `seq`
                      rnf gExifKwdCnt    `seq`
                      rnf gExifFlashSrc

instance Default GroupExif where
  def = GroupExif Map.empty Map.empty Map.empty Map.empty
        Map.empty Map.empty Map.empty Map.empty Map.empty Map.empty
        Map.empty Map.empty Map.empty

exifLocalCreateDate :: Exif -> Maybe LocalTime
exifLocalCreateDate = (zonedTimeToLocalTime . etTime <$>) . exifCreateDate

-- | Expands a group exif with a single exif
addExifToGroup :: GroupExif -> Exif -> GroupExif
addExifToGroup g Exif{..} =
  g { gExifPeople    = foldSet (gExifPeople    g) exifPeople
    , gExifKeywords  = foldSet (gExifKeywords  g) exifKeywords
    , gExifCountries = count1  (gExifCountries g) exifCountry
    , gExifProvinces = count1  (gExifProvinces g) exifProvince
    , gExifCities    = count1  (gExifCities    g) exifCity
    , gExifLocations = count1  (gExifLocations g) exifLocation
    , gExifCameras   = count1  (gExifCameras   g) exifCamera
    , gExifLenses    = count1  (gExifLenses    g) (Just $ liName exifLens)
    , gExifTitles    = count1  (gExifTitles    g) exifTitle
    , gExifCaptions  = count1  (gExifCaptions  g) exifCaption
    , gExifPeopleCnt = count1  (gExifPeopleCnt g) (setSz exifPeople)
    , gExifKwdCnt    = count1  (gExifKwdCnt    g) (setSz exifKeywords)
    , gExifFlashSrc  = count1  (gExifFlashSrc  g) (fiSource exifFlashInfo)
    }
    where count1 :: (Ord k, Num v) => Map.Map k v -> k -> Map.Map k v
          count1 m k = Map.insertWith (+) k 1 m
          foldSet m s = if Set.null s
                          then m `count1` Nothing
                          else foldl' count1 m (Set.map Just s)
          setSz = Just . Set.size

instance Semigroup GroupExif where
  g1 <> g2 =
    GroupExif
    { gExifPeople    = gExifPeople    g1 `merge` gExifPeople    g2
    , gExifKeywords  = gExifKeywords  g1 `merge` gExifKeywords  g2
    , gExifCountries = gExifCountries g1 `merge` gExifCountries g2
    , gExifProvinces = gExifProvinces g1 `merge` gExifProvinces g2
    , gExifCities    = gExifCities    g1 `merge` gExifCities    g2
    , gExifLocations = gExifLocations g1 `merge` gExifLocations g2
    , gExifCameras   = gExifCameras   g1 `merge` gExifCameras   g2
    , gExifLenses    = gExifLenses    g1 `merge` gExifLenses    g2
    , gExifTitles    = gExifTitles    g1 `merge` gExifTitles    g2
    , gExifCaptions  = gExifCaptions  g1 `merge` gExifCaptions  g2
    , gExifPeopleCnt = gExifPeopleCnt g1 `merge` gExifPeopleCnt g2
    , gExifKwdCnt    = gExifKwdCnt    g1 `merge` gExifKwdCnt    g2
    , gExifFlashSrc  = gExifFlashSrc  g1 `merge` gExifFlashSrc  g2
    }
    where
      merge :: (Ord k, Num v) => Map.Map k v -> Map.Map k v -> Map.Map k v
      merge = Map.unionWith (+)

unknown :: Text
unknown = "unknown"

data Rotate = RCenter
            | RLeft
            | RRight

rotateToJSON :: Rotate -> Int
rotateToJSON RCenter =  0
rotateToJSON RLeft   = -1
rotateToJSON RRight  =  1

data Transform = Transform
                   Rotate
                   Bool -- Flip X.
                   Bool -- Flip Y.

instance Default Transform where
  def = Transform RCenter False False

transformParams :: Transform -> (Int, Bool, Bool)
transformParams (Transform r fx fy) =
  (rotateToJSON r, fx, fy)

transformMatrix :: Transform -> (Double, Double, Double, Double)
transformMatrix (Transform r fx fy) =
  let radians = case r of
        RCenter -> 0
        RLeft   -> pi * 3 /2
        RRight  -> pi / 2
      r_cos = cos radians
      r_sin = sin radians
      scale_x = if fx then -1 else 1
      scale_y = if fy then -1 else 1
  in (r_cos * scale_x, r_sin, -r_sin, r_cos * scale_y)

-- | Parses the (Lightroom-specific?) hierarchical keywords.
parseHierSubject :: Text -> [Text]
parseHierSubject = Text.splitOn "|"

extractFirstLastName :: Text -> Maybe (Text, Text)
extractFirstLastName name
  | [l, f] <- Text.splitOn "/" name,
    not (Text.null l),
    not (Text.null f) = Just (f, l)
  | [f, l] <- Text.splitOn " " name,
    not (Text.null l),
    not (Text.null f) = Just (f, l)
  | otherwise = Nothing

-- | Formats a \"Doe\/John\" name as \"John Doe\" or \"John D.\".
formatPerson :: Bool -> Text -> Text
formatPerson abbrev name =
  case extractFirstLastName name of
    Just (firstn, lastn) ->
      sformat (stext % " " % stext) firstn
        (if abbrev then Text.head lastn `Text.cons` "." else lastn)
    _ -> name

-- | Returns the (partial) affine matrix for the given orientation.
affineTransform :: Orientation -> Transform
affineTransform OrientationTopLeft  = Transform RCenter False False
affineTransform OrientationTopRight = Transform RCenter True  False
affineTransform OrientationBotRight = Transform RCenter True  True
affineTransform OrientationBotLeft  = Transform RCenter False True
affineTransform OrientationLeftTop  = Transform RLeft   False True
affineTransform OrientationRightTop = Transform RRight  False False
affineTransform OrientationRightBot = Transform RRight  False True
affineTransform OrientationLeftBot  = Transform RLeft   False False

-- | Tries to parse a value as is, or as a list with single element.
parseValOrList :: Value -> Parser Integer
parseValOrList val =
  parseJSON val <|>
  parseSingletonList val <|>
  parseIsoString val

-- | Parse various formats for the ISO string.
--
-- The "i, 0, 0" format was generated by some old Android versions.
parseIsoString :: Value -> Parser Integer
parseIsoString =
  withText "ISO String" $ \t -> do
  let w = Text.words t
      fmsg = fail $ "Unknown format for ISO string: '" ++
                    Text.unpack t ++ "'"
  case w of
    [i]    -> readInt i
    -- TODO: record high iso mode.
    [_, i] -> readInt i
    _      -> maybe fmsg readInt $ ", 0, 0" `Text.stripSuffix` t

readInt :: Text -> Parser Integer
readInt t =
  case Text.decimal t of
    Right (i, "") -> return i
    Right (_, _)  -> fail $ "Leftover chars while parsing '" ++
                     Text.unpack t ++ "' as integer"
    Left s        -> fail s

parseSingletonList :: (FromJSON a) => Value -> Parser a
parseSingletonList val = do
  lst <- parseJSON val
  case lst of
    [e] -> return e
    _   -> fail "Not a single-element list"

exifFromRaw :: Config -> RawExif -> Exif
exifFromRaw config RawExif{..} = flip evalState Set.empty $ do
  let logger s = modify (s `Set.insert`)
      loggerN s = logger s >> return Nothing
      evalV p e = maybe (return Nothing)
                        (\v' -> if p v'
                                then loggerN (e v')
                                else return $ Just v')
      checkNull f = evalV Text.null (const $ "Empty " <> f <> " information")
      pPeople = cfgPeoplePrefix config
      pIgnore = cfgIgnorePrefix config
      dropIgnored = filter (not . (pIgnore `Text.isPrefixOf`))
      subjPeople      = foldl' (\e ks ->
                                 case ks of
                                   x:p | x == pPeople -> p ++ e
                                   _                  -> e) [] rExifHSubjects
      exifPeople = Set.fromList $ rExifPeople ++ subjPeople
      dropPeople = filter (not . (`Set.member` exifPeople))
      exifKeywords    = Set.fromList $
                        foldl' (\e ks ->
                                  case ks of
                                    x:_ | x /= pPeople ->
                                          (dropPeople . dropIgnored) ks ++ e
                                    _ -> e) [] rExifHSubjects
      exifCamera       = maybe rExifModel
                         (\s ->
                             Just $ case rExifModel of
                                      Nothing  -> "#" `Text.append` s
                                      Just txt -> Text.concat [txt, " (#", s, ")"]
                         ) rExifSerial
      exifLens         = fromMaybe unknownLens rExifLens
      exifOrientation  = fromMaybe OrientationTopLeft rExifOrientation
      exifCreateDate   = rExifCreateDate
      exifAperture     = rExifAperture
      exifFocalLength  = rExifFocalLength
      exifFL35mm       = rExifFL35mm
      exifISO          = rExifISO
      exifSSpeedDesc   = rExifSSpeedDesc
      exifSSpeedVal    = rExifSSpeedVal
      exifMimeType     = rExifMimeType
      exifRating       = rExifRating
      flashSource      = rExifFlashSource >>= parseFlashSource
      flashMode        = rExifFlashMode
      exifFlashInfo    = FlashInfo flashSource flashMode
  exifModel        <- checkNull "model" rExifModel
  exifSerial       <- checkNull "serial" rExifSerial
  exifTitle        <- checkNull "title" rExifTitle
  exifCaption      <- checkNull "caption" rExifCaption
  exifCountry      <- checkNull "country" rExifCountry
  exifProvince     <- checkNull "province" rExifProvince
  exifCity         <- checkNull "city" rExifCity
  exifLocation     <- checkNull "location" rExifLocation
  exifShutterCount <- evalV (> tooHighShutterCount)
                      (sformat ("Unlikely shutter count: " % int))
                      rExifShutterCount
  errs <- get
  let exifWarning      = maybe errs (`Set.insert` errs) rExifWarning
  return Exif{..}

-- | Promotion rules for file to exif
promoteFileExif :: Maybe Exif -> Maybe Exif -> [Exif] -> Maybe Exif -> [Exif] -> Exif
promoteFileExif re se je mm me =
  let setmerge :: (Ord a) => (Exif -> Set a) -> Set a
      setmerge fn = Set.unions $ maybe Set.empty fn re:
                               maybe Set.empty fn se:
                               map fn je ++
                               maybe Set.empty fn mm:
                               map fn me
      fjust :: (Exif -> Maybe a) -> Maybe a
      fjust fn = msum $ [re >>= fn, se >>= fn] ++ map fn je ++ [mm >>= fn] ++ map fn me
      skipMaybes :: (Exif -> a) -> [a]
      skipMaybes fn = catMaybes [fn <$> re, fn <$> se] ++ map fn je ++
                      catMaybes [fn <$> mm] ++ map fn me
      skipUnknown = filter (/= unknown)
      fjust' :: (Exif -> a) -> a -> a
      fjust' fn d = case skipMaybes fn of
                      []  -> d
                      x:_ -> x
      fno_u :: (Exif -> Text) -> Text -> Text
      fno_u fn d = case skipUnknown (skipMaybes fn) of
                      []  -> d
                      x:_ -> x
      exifPeople'       = setmerge exifPeople
      exifKeywords'     = setmerge exifKeywords `Set.difference` exifPeople'
      exifCountry'      = fjust  exifCountry
      exifProvince'     = fjust  exifProvince
      exifCity'         = fjust  exifCity
      exifLocation'     = fjust  exifLocation
      exifCamera'       = fjust  exifCamera
      exifModel'        = fjust  exifModel
      exifSerial'       = fjust  exifSerial
      liName'           = fno_u (liName <$> exifLens) unknown
      liSpec'           = fno_u (liSpec <$> exifLens) unknown
      liName''          = if "Unknown" `Text.isPrefixOf` liName' && liSpec' /= unknown
                          then liSpec'
                          else liName'
      liFL'             = fjust (liFL     <$> exifLens)
      liAperture'       = fjust (liAp     <$> exifLens)
      liSerial'         = fjust (liSerial <$> exifLens)
      exifLens'         = LensInfo liName'' liSpec' liFL' liAperture' liSerial'
      exifOrientation'  = fjust' exifOrientation OrientationTopLeft
      exifCreateDate'   = fjust  exifCreateDate
      exifAperture'     = fjust  exifAperture
      exifFocalLength'  = fjust  exifFocalLength
      exifFL35mm'       = fjust  exifFL35mm
      exifISO'          = fjust  exifISO
      exifSSpeedDesc'   = fjust  exifSSpeedDesc
      exifSSpeedVal'    = fjust  exifSSpeedVal
      exifShutterCount' = fjust  exifShutterCount
      -- TODO: both title and caption (and other fields) could differ
      -- between various versions. How to expose this in viewer?
      exifTitle'        = fjust  exifTitle
      exifCaption'      = fjust  exifCaption
      -- Mime type cannot be promoted, since various component _will_ have various types.
      exifMimeType'     = Nothing
      exifRating'       = fjust  exifRating
      exifWarning'      = setmerge exifWarning
      flashSource       = fjust  (fiSource . exifFlashInfo)
      flashMode         = fjust  (fiMode . exifFlashInfo)
      exifFlashInfo'    = FlashInfo flashSource flashMode
  in Exif { exifPeople       = exifPeople'
          , exifKeywords     = exifKeywords'
          , exifCountry      = exifCountry'
          , exifProvince     = exifProvince'
          , exifCity         = exifCity'
          , exifLocation     = exifLocation'
          , exifCamera       = exifCamera'
          , exifModel        = exifModel'
          , exifSerial       = exifSerial'
          , exifLens         = exifLens'
          , exifOrientation  = exifOrientation'
          , exifCreateDate   = exifCreateDate'
          , exifTitle        = exifTitle'
          , exifCaption      = exifCaption'
          , exifAperture     = exifAperture'
          , exifFocalLength  = exifFocalLength'
          , exifFL35mm       = exifFL35mm'
          , exifISO          = exifISO'
          , exifSSpeedDesc   = exifSSpeedDesc'
          , exifSSpeedVal    = exifSSpeedVal'
          , exifShutterCount = exifShutterCount'
          , exifMimeType     = exifMimeType'
          , exifRating       = exifRating'
          , exifWarning      = exifWarning'
          , exifFlashInfo    = exifFlashInfo'
          }

-- TODO: make this saner/ensure it's canonical path.
buildPath :: FilePath -> FilePath -> FilePath
buildPath dir name = dir ++ "/" ++ name

-- | Writes the (binary) exif cache for a given file.
writeBExif :: Config -> FilePath -> EExif -> IO ()
writeBExif config path =
  writeCacheFile config path bExifPath . Data.Store.encode

-- | Tries to read the (binary) exif cache for a given file.
readBExif :: Config -> FilePath -> IO (Maybe EExif)
readBExif config path = do
  contents <- readCacheFile config path bExifPath True [exifPath config path]
  return $ contents >>= either (const Nothing) Just . Data.Store.decode

-- | Writes the exif caches (raw and binary) for a given file.
writeExifs :: Config -> FilePath -> ERawExif -> IO (FilePath, EExif)
writeExifs config dir er = do
  let rpath = either freFilePath (Text.unpack . rExifSrcFile) er
      fpath = buildPath dir rpath
      ro    = either (fmap Data.Aeson.encode . freValue)
                     (Just . Data.Aeson.encode . rExifRaw) er
      e     = bimap freMessage (exifFromRaw config) er
  maybe (return ()) (writeCacheFile config fpath exifPath) ro
  writeBExif config fpath e
  return (rpath, e)

-- | Tries to read the (raw) exif cache for a given file.
readExif :: Config -> FilePath -> IO (Maybe (Either Text RawExif))
readExif config path = do
  contents <- readCacheFile config path exifPath True []
  return $ contents >>= Just . first Text.pack . eitherDecodeStrict'

-- | Try to get an exif value for a path, either from cache or from filesystem.
getExif :: Config -> FilePath -> [FilePath] -> IO (Int, Map Text EExif)
getExif config dir paths = do
  (cache1, m1) <- foldM (\(c, m) p -> do
                            let fpath = buildPath dir p
                            exif <- readBExif config fpath
                            case exif of
                              Nothing -> return (c, p:m)
                              Just e  -> return (Map.insert (Text.pack p) e c, m)
                        ) (Map.empty, []) paths
  (cache2, m2) <- foldM (\(c, m) p -> do
                            let fpath = buildPath dir p
                            exif <- readExif config fpath
                            case exif of
                              -- failed to find file, or file stale.
                              Nothing -> return (c, p:m)
                              -- found file, but parsing might have failed.
                              Just v -> do
                                let e = second (exifFromRaw config) v
                                writeBExif config fpath e
                                return (Map.insert (Text.pack p) e c, m)
                 ) (cache1, []) m1
  jsons <- if null m2
             then return []
             else do
               -- TODO: fix this. It is very ugly, catches _all_
               -- exceptions, but it's the only way I found to
               -- reliably disable the slowloris protection. There a
               -- quite a few issues on the wai project regarding the
               -- timeout reaper, but without conclusive
               -- solutions. See
               -- https://github.com/yesodweb/wai/issues/351 for
               -- example.
               exifs <- (parseExifs <$> extractExifs dir m2) `catch`
                 (\e -> let e' = sformat shown (e :: SomeException)
                        in putStrLn ("Error: " ++ e') >> return (Left e'))
               return $ case exifs of
                          Left msg -> map (\p ->
                                             let freFilePath = p
                                                 freMessage = msg
                                                 freValue = Nothing
                                             in Left FailRExif{..}) m2
                          Right rs -> rs
  let expensive = length m2
  cache3 <- foldM (\m r -> do
                      (path, e) <- writeExifs config dir r
                      return $ Map.insert (Text.pack path) e m
                  ) cache2 jsons
  return (expensive, cache3)

exifPath :: Config -> FilePath -> FilePath
exifPath config path =
  cachedBasename config path ("exif" ++ devSuffix)

bExifPath :: Config -> FilePath -> FilePath
bExifPath config path =
  cachedBasename config path ("bexif" ++ devSuffix)

extractExifs :: FilePath -> [FilePath] -> IO BS.ByteString
extractExifs dir paths = withSystemTempFile "corydalis-exif" $ \fpath fhandle -> do
  -- TODO: should instead make exiftool write directly to individual raw exifs?
  let args = [
        "-json",
        "-struct",
        "-l"
        ] ++ paths
      pconfig =
        setStdin closed
        . setStdout (useHandleClose fhandle)
        . setStderr closed  -- TODO: this failes the writes, not sure if OK
        . setCloseFds True
        . setWorkingDir dir
        $ proc "exiftool" args
  -- Note: exiftool exits with non-zero exit code if errors happened,
  -- but still generates valid JSON output. So we ignore the exit code
  -- completely, as we can't attribute the error to a specific
  -- file. That should be rather done via 'Error' entry in the object.
  _ <- runProcess pconfig
  BS.readFile fpath

parseExifs :: BS.ByteString -> Either Text [ERawExif]
parseExifs bs =
  case eitherDecodeStrict' bs of
    Left msg   -> Left $ Text.pack msg
    Right vals -> Right $ mapMaybe parseExif vals

parseError :: Value -> Parser (FilePath, Text)
parseError = withObject "exiftool result" $ \o -> do
  fpath <- o .: "SourceFile"
  err <- o .~: "Error"
  return (fpath, err)

parseSrcFile :: Value -> Parser FilePath
parseSrcFile = withObject "exiftool result" (.: "SourceFile")

parseExif :: Value -> Maybe ERawExif
parseExif val =
  case parseEither parseError val of
    Right (fp, msg) -> Just . Left $ FailRExif fp msg (Just val)
    Left _ -> case parseEither parseJSON val of
                Left msg -> parseMaybe parseSrcFile val >>=
                            \fp -> return . Left $
                                   FailRExif fp (Text.pack msg) (Just val)
                Right r  -> Just $ Right r

-- | Tries to compute the date the image was taken.
parseCreateDate :: Object -> Parser (Maybe ExifTime)
parseCreateDate o = do
  dto  <- msum $ map (o .!:) [ "SubSecDateTimeOriginal"
                             , "DateTimeOriginal"
                             , "SubSecCreateDate"
                             , "CreateDate"
                             ]
  -- Note: Aeson does have parsing of time itself, but only support
  -- %Y-%m-%d, whereas exiftool (or exif spec itself?) outputs in
  -- %Y:%m:%d format, so we have to parse the time manually.
  let dto' = do -- in Maybe monad
        dateinfo <- dto
        msum $ map (\fmt -> parseTimeM True defaultTimeLocale fmt dateinfo)
               [ "%Y:%m:%d %T%Q%Z"
               , "%Y:%m:%d %T%Q"
               , "%Y:%m:%d %T"
               ]
  return $ ExifTime <$> dto'

$(makeStore ''ExifTime)
$(makeStore ''FlashSource)
$(makeStore ''FlashInfo)
$(makeStore ''Exif)
$(makeStore ''GroupExif)
