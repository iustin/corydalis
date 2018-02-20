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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Exif ( Exif(..)
            , GroupExif(..)
            , Rotate(..)
            , Transform(..)
            , LensInfo(..)
            , LensFocalLength(..)
            , LensAperture(..)
            , getExif
            , promoteFileExif
            , addExifToGroup
            , affineTransform
            , unknown
            , unknownLens
            ) where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception.Base
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types       (Parser, modifyFailure)
import qualified Data.ByteString        as BS (ByteString, readFile)
import           Data.Default
import           Data.List
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Maybe
import           Data.Scientific        (toBoundedInteger)
import           Data.Semigroup
import           Data.Set               (Set)
import qualified Data.Set               as S
import           Data.Store
import           Data.Store.TH          (makeStore)
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Time.Format
import           Data.Time.LocalTime
import           Prelude
import           System.IO.Temp
import           System.Process.Typed

import           Cache
import           Compat.Orphans         ()
import           Settings.Development
import           Types

data Orientation
  = OrientationTopLeft
  | OrientationTopRight
  | OrientationBotRight
  | OrientationBotLeft
  | OrientationLeftTop
  | OrientationRightTop
  | OrientationRightBot
  | OrientationLeftBot
  deriving (Show)

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
      Just v  -> fail $ "Invalid orientation value '" ++ show v ++ "'"
      Nothing -> fail $ "Non-integer orientation value '" ++ show n ++ "'"

$(makeStore ''Orientation)

extractRaw :: (FromJSON a) => Text -> Value -> Parser a
extractRaw t =
  modifyFailure (\s -> "Parsing " ++ desc ++ ": " ++ s) .
  withObject (desc ++ "/num|val") (\o -> o .: "num" <|> o .: "val")
  where desc = T.unpack t

extractParsed :: (FromJSON a) => Text -> Value -> Parser a
extractParsed t =
  modifyFailure (\s -> "Parsing " ++ desc ++ ": " ++ s) .
  withObject (desc ++ "/val") (.: "val")
  where desc = T.unpack t

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
  { liName :: !Text
  , liSpec :: !Text
  , liFL   :: !(Maybe LensFocalLength)
  , liAp   :: !(Maybe LensAperture)
  } deriving (Show, Eq, Ord)

$(makeStore ''LensInfo)

instance NFData LensInfo where
  rnf LensInfo{..} = rnf liName `seq`
                     rnf liSpec `seq`
                     rnf liFL   `seq`
                     rnf liAp

unknownLens :: LensInfo
unknownLens = LensInfo unknown unknown Nothing Nothing

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
  return LensInfo{..}

data RawExif = RawExif
  { rExifSrcFile     :: Text
  , rExifCamera      :: Maybe Text
  , rExifSerial      :: Maybe Text
  , rExifLens        :: Maybe LensInfo
  , rExifOrientation :: Maybe Orientation
  , rExifHSubjects   :: [[Text]]
  , rExifPeople      :: [Text]
  , rExifCountry     :: Maybe Text
  , rExifProvince    :: Maybe Text
  , rExifCity        :: Maybe Text
  , rExifLocation    :: Maybe Text
  , rExifCreateDate  :: Maybe LocalTime
  , rExifTitle       :: Maybe Text
  , rExifCaption     :: Maybe Text
  , rExifAperture    :: Maybe Double
  , rExifFocalLength :: Maybe Double
  , rExifFL35mm      :: Maybe Double
  , rExifRaw         :: Object
  } deriving (Show)

instance FromJSON RawExif where
  parseJSON = withObject "RawExif" $ \o -> do
    let giveUp        = pure Nothing
    rExifSrcFile     <- o .: "SourceFile"
    rExifCamera      <- o .!:? "Model"
    rExifLens        <- (Just <$> lensInfoFromObject o) <|>
                        giveUp
    rExifSerial      <- o .!:? "Serial"
    rExifOrientation <- o .!:? "Orientation"
    hsubjs           <- o .!:? "HierarchicalSubject" .!= []
    let rExifHSubjects = map parseHierSubject hsubjs
    rExifPeople      <- o .!:? "PersonInImage" .!= []
    rExifCountry     <- o .~:? "Country"
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
    rExifFL35mm      <- o .!:? "FocalLengthIn35mmFormat"
    let rExifRaw      = o
    return RawExif{..}

data Exif = Exif
  { exifPeople      :: !(Set Text)
  , exifKeywords    :: !(Set Text)
  , exifCountry     :: !(Maybe Text)
  , exifProvince    :: !(Maybe Text)
  , exifCity        :: !(Maybe Text)
  , exifLocation    :: !(Maybe Text)
  , exifCamera      :: !Text
  , exifSerial      :: !Text
  , exifLens        :: !LensInfo
  , exifOrientation :: !Orientation
  , exifCreateDate  :: !(Maybe LocalTime)
  , exifTitle       :: !(Maybe Text)
  , exifCaption     :: !(Maybe Text)
  , exifAperture    :: !(Maybe Double)
  , exifFocalLength :: !(Maybe Double)
  , exifFL35mm      :: !(Maybe Double)
  } deriving (Show)

instance NFData Exif where
  rnf Exif{..} = rnf exifPeople     `seq`
                 rnf exifKeywords   `seq`
                 rnf exifCountry    `seq`
                 rnf exifProvince   `seq`
                 rnf exifCity       `seq`
                 rnf exifLocation   `seq`
                 rnf exifCamera     `seq`
                 rnf exifSerial     `seq`
                 rnf exifLens       `seq`
                 rnf exifCreateDate `seq`
                 rnf exifTitle      `seq`
                 rnf exifCaption    `seq`
                 rnf exifAperture   `seq`
                 rnf exifFocalLength `seq`
                 rnf exifFL35mm

type NameStats = Map Text Integer

data GroupExif = GroupExif
  { gExifPeople    :: !NameStats
  , gExifKeywords  :: !NameStats
  , gExifCountries :: !NameStats
  , gExifProvinces :: !NameStats
  , gExifCities    :: !NameStats
  , gExifLocations :: !NameStats
  , gExifCameras   :: !NameStats
  , gExifLenses    :: !NameStats
  } deriving (Show)

instance NFData GroupExif where
  rnf GroupExif{..} = rnf gExifPeople    `seq`
                      rnf gExifKeywords  `seq`
                      rnf gExifCountries `seq`
                      rnf gExifProvinces `seq`
                      rnf gExifCities    `seq`
                      rnf gExifLocations `seq`
                      rnf gExifCameras   `seq`
                      rnf gExifLenses

instance Default GroupExif where
  def = GroupExif M.empty M.empty M.empty M.empty M.empty M.empty M.empty M.empty

-- | Expands a group exif with a single exif
addExifToGroup :: GroupExif -> Exif -> GroupExif
addExifToGroup g e =
  g { gExifPeople    = foldl' count (gExifPeople    g) (exifPeople   e)
    , gExifKeywords  = foldl' count (gExifKeywords  g) (exifKeywords e)
    , gExifCountries = foldl' count (gExifCountries g) (exifCountry  e)
    , gExifProvinces = foldl' count (gExifProvinces g) (exifProvince e)
    , gExifCities    = foldl' count (gExifCities    g) (exifCity     e)
    , gExifLocations = foldl' count (gExifLocations g) (exifLocation e)
    , gExifCameras   = count (gExifCameras g) (exifCamera e)
    , gExifLenses    = count (gExifLenses  g) (liName $ exifLens e)
    }
    where count m k = M.insertWith (+) k 1 m

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
    }
    where merge = M.unionWith (+)

unknown :: Text
unknown = "unknown"

data Rotate = RCenter
            | RLeft
            | RRight

data Transform = Transform
                   Rotate
                   Bool -- Flip X.
                   Bool -- Flip Y.

instance Default Transform where
  def = Transform RCenter False False

-- | Parses the (Lightroom-specific?) hierarchical keywords.
parseHierSubject :: Text -> [Text]
parseHierSubject = T.splitOn "|"

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

exifFromRaw :: Config -> RawExif -> Exif
exifFromRaw config RawExif{..} =
  let pPeople = cfgPeoplePrefix config
      pIgnore = cfgIgnorePrefix config
      dropIgnored = filter (not . (pIgnore `T.isPrefixOf`))
      subjPeople      = foldl' (\e ks ->
                                 case ks of
                                   x:p | x == pPeople -> p ++ e
                                   _   -> e) [] rExifHSubjects
      exifPeople = S.fromList $ rExifPeople ++ subjPeople
      dropPeople = filter (not . (`S.member` exifPeople))
      exifKeywords    = S.fromList $
                        foldl' (\e ks ->
                                  case ks of
                                    x:_ | x /= pPeople ->
                                          (dropPeople . dropIgnored) ks ++ e
                                    _ -> e) [] rExifHSubjects
      exifCountry     = rExifCountry
      exifProvince    = rExifProvince
      exifCity        = rExifCity
      exifLocation    = rExifLocation
      exifCamera      = fromMaybe unknown rExifCamera
      exifLens        = fromMaybe unknownLens rExifLens
      exifSerial      = fromMaybe unknown rExifSerial
      exifOrientation = fromMaybe OrientationTopLeft rExifOrientation
      exifCreateDate  = rExifCreateDate
      exifTitle       = rExifTitle
      exifCaption     = rExifCaption
      exifAperture    = rExifAperture
      exifFocalLength = rExifFocalLength
      exifFL35mm      = rExifFL35mm
  in Exif{..}

-- | Promotion rules for file to exif
promoteFileExif :: Maybe Exif -> Maybe Exif -> [Exif] -> Exif
promoteFileExif re se je =
  let setmerge :: (Ord a) => (Exif -> Set a) -> Set a
      setmerge fn = S.unions $ maybe S.empty fn re:
                               maybe S.empty fn se:
                               map fn je
      first :: (Exif -> Maybe a) -> Maybe a
      first fn = msum $ [re >>= fn, se >>= fn] ++ map fn je
      skipMaybes :: (Exif -> a) -> [a]
      skipMaybes fn = catMaybes [fn <$> re, fn <$> se] ++ map fn je
      first' :: (Exif -> a) -> a -> a
      first' fn d = case skipMaybes fn of
                      []  -> d
                      x:_ -> x
      exifPeople'      = setmerge exifPeople
      exifKeywords'    = setmerge exifKeywords `S.difference` exifPeople'
      exifCountry'     = first  exifCountry
      exifProvince'    = first  exifProvince
      exifCity'        = first  exifCity
      exifLocation'    = first  exifLocation
      exifCamera'      = first' exifCamera unknown
      exifSerial'      = first' exifSerial unknown
      liName'          = first' (liName <$> exifLens) unknown
      liSpec'          = first' (liSpec <$> exifLens) unknown
      liFL'            = first (liFL   <$> exifLens)
      liAperture'      = first (liAp   <$> exifLens)
      exifLens'        = LensInfo liName' liSpec' liFL' liAperture'
      exifOrientation' = first' exifOrientation OrientationTopLeft
      exifCreateDate'  = first  exifCreateDate
      exifAperture'    = first  exifAperture
      exifFocalLength' = first  exifFocalLength
      exifFL35mm'      = first  exifFL35mm
      -- TODO: both title and caption (and other fields) could differ
      -- between various versions. How to expose this in viewer?
      exifTitle'       = first  exifTitle
      exifCaption'     = first  exifCaption
  in Exif { exifPeople      = exifPeople'
          , exifKeywords    = exifKeywords'
          , exifCountry     = exifCountry'
          , exifProvince    = exifProvince'
          , exifCity        = exifCity'
          , exifLocation    = exifLocation'
          , exifCamera      = exifCamera'
          , exifSerial      = exifSerial'
          , exifLens        = exifLens'
          , exifOrientation = exifOrientation'
          , exifCreateDate  = exifCreateDate'
          , exifTitle       = exifTitle'
          , exifCaption     = exifCaption'
          , exifAperture    = exifAperture'
          , exifFocalLength = exifFocalLength'
          , exifFL35mm      = exifFL35mm'
          }

-- TODO: make this saner/ensure it's canonical path.
buildPath :: FilePath -> FilePath -> FilePath
buildPath dir name = dir ++ "/" ++ name

-- | Writes the (binary) exif cache for a given file.
writeBExif :: Config -> FilePath -> Exif -> IO ()
writeBExif config path =
  writeCacheFile config path bExifPath . Data.Store.encode

-- | Tries to read the (binary) exif cache for a given file.
readBExif :: Config -> FilePath -> IO (Maybe Exif)
readBExif config path = do
  contents <- readCacheFile config path bExifPath True [exifPath config path]
  case contents of
    Nothing -> return Nothing
    Just c ->
      return $ case Data.Store.decode c of
                 Left _  -> Nothing
                 Right v -> Just v

-- | Writes the exif caches (raw and binary) for a given file.
writeExifs :: Config -> FilePath -> RawExif -> IO (FilePath, Exif)
writeExifs config dir r = do
  let rpath = T.unpack $ rExifSrcFile r
      fpath = buildPath dir rpath
  writeCacheFile config fpath exifPath (Data.Aeson.encode (rExifRaw r))
  let e = exifFromRaw config r
  writeBExif config fpath e
  return (rpath, e)

-- | Tries to read the (raw) exif cache for a given file.
readExif :: Config -> FilePath -> IO (Maybe RawExif)
readExif config path = do
  contents <- readCacheFile config path exifPath True []
  return $ contents >>= decodeStrict'

-- | Try to get an exif value for a path, either from cache or from filesystem.
getExif :: Config -> FilePath -> [FilePath] -> IO (Map FilePath Exif)
getExif config dir paths = do
  (cache1, m1) <- foldM (\(c, m) p -> do
                            let fpath = buildPath dir p
                            exif <- readBExif config fpath
                            case exif of
                              Nothing -> return (c, p:m)
                              Just e  -> return (M.insert p e c, m)
                        ) (M.empty, []) paths
  (cache2, m2) <- foldM (\(c, m) p -> do
                            let fpath = buildPath dir p
                            exif <- readExif config fpath
                            case exif of
                              Nothing -> return (c, p:m)
                              Just v -> do
                                let e = exifFromRaw config v
                                writeBExif config fpath e
                                return (M.insert p e c, m)
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
                 (\e -> putStrLn ("Error: " ++ show (e :: SomeException)) >> return Nothing)
               return $ fromMaybe [] exifs
  foldM (\m r -> do
            (path, e) <- writeExifs config dir r
            return $ M.insert path e m
        ) cache2 jsons

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

parseExifs :: BS.ByteString -> Maybe [RawExif]
parseExifs = decodeStrict'

-- | Tries to compute the date the image was taken.
parseCreateDate :: Object -> Parser (Maybe LocalTime)
parseCreateDate o = do
  dto  <- msum $ map (o .!:) [ "SubSecDateTimeOriginal"
                             , "DateTimeOriginal"
                             , "SubSecCreateDate"
                             , "CreateDate"
                             ]
  -- Note: Aeson does have parsing of time itself, but only support
  -- %Y-%m-%d, whereas exiftool (or exif spec itself?) outputs in
  -- %Y:%m:%d format, so we have to parse the time manually.
  let dto' = dto >>= parseTimeM True defaultTimeLocale "%Y:%m:%d %T%Q"
  return dto'

$(makeStore ''Exif)
