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
{-# LANGUAGE TemplateHaskell #-}

module Exif ( Exif(..)
            , GroupExif(..)
            , Rotate(..)
            , Transform(..)
            , getExif
            , promoteFileExif
            , addExifToGroup
            , affineTransform
            , unknown
            ) where

import Types
import Cache
import Settings.Development

import Prelude
import Control.DeepSeq
import Data.Aeson.Types (Parser)
import Data.Default
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.ByteString as BS (ByteString, readFile)
import qualified Data.ByteString.Lazy as BSL (toStrict)
import Data.Aeson
import Data.List
import Data.Semigroup
import Data.Store ()
import Data.Store.TH (makeStore)
import Data.Scientific (toBoundedInteger)
import Data.Time.LocalTime

import Control.Applicative
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Maybe
import System.Posix.Files hiding (fileSize)
import System.Process.Typed
import Data.Store
import Data.Time.Format

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

data RawExif = RawExif
  { rExifSrcFile     :: Text
  , rExifCamera      :: Maybe Text
  , rExifSerial      :: Maybe Text
  , rExifLens        :: Maybe Text
  , rExifOrientation :: Maybe Orientation
  , rExifHSubjects   :: [[Text]]
  , rExifCountry     :: Maybe Text
  , rExifState       :: Maybe Text
  , rExifProvince    :: Maybe Text
  , rExifCity        :: Maybe Text
  , rExifCreateDate  :: Maybe LocalTime
  , rExifRaw         :: Object
  } deriving (Show)

instance FromJSON RawExif where
  parseJSON = withObject "RawExif" $ \o -> do
    rExifSrcFile     <- o .: "SourceFile"
    rExifCamera      <- o .:? "Model"
    rExifLens        <- o .: "LensModel" <|>
                        o .: "LensID"    <|>
                        o .: "Lens"      <|>
                      pure Nothing
    rExifSerial      <- o .:? "Serial"
    rExifOrientation <- o .:? "Orientation"
    hsubjs           <- o .:? "HierarchicalSubject" .!= []
    let rExifHSubjects = map parseHierSubject hsubjs
    rExifCountry     <- o .:? "Country"
    rExifState       <- o .:? "State"
    rExifProvince    <- o .:? "Province-State"
    rExifCity        <- o .:? "City"
    rExifCreateDate  <- parseCreateDate o
    let rExifRaw      = o
    return RawExif{..}

data Exif = Exif
  { exifPeople      :: ![Text]
  , exifKeywords    :: ![Text]
  , exifLocations   :: ![Text]
  , exifCamera      :: !Text
  , exifSerial      :: !Text
  , exifLens        :: !Text
  , exifOrientation :: !Orientation
  , exifCreateDate  :: !(Maybe LocalTime)
  } deriving (Show)

instance NFData Exif where
  rnf Exif{..} = rnf exifPeople    `seq`
                 rnf exifKeywords  `seq`
                 rnf exifLocations `seq`
                 rnf exifCamera    `seq`
                 rnf exifSerial    `seq`
                 rnf exifLens      `seq`
                 rnf exifCreateDate

data GroupExif = GroupExif
  { gExifPeople      :: !(Map Text Integer)
  , gExifKeywords    :: !(Map Text Integer)
  , gExifLocations   :: !(Map Text Integer)
  , gExifCameras     :: !(Map Text Integer)
  , gExifLenses      :: !(Map Text Integer)
  } deriving (Show)

instance NFData GroupExif where
  rnf GroupExif{..} = rnf gExifPeople    `seq`
                      rnf gExifKeywords  `seq`
                      rnf gExifLocations `seq`
                      rnf gExifCameras   `seq`
                      rnf gExifLenses

instance Default GroupExif where
  def = GroupExif M.empty M.empty M.empty M.empty M.empty

-- | Expands a group exif with a single exif
addExifToGroup :: GroupExif -> Exif -> GroupExif
addExifToGroup g e =
  g { gExifPeople    = foldl' count (gExifPeople g) (exifPeople e)
    , gExifKeywords  = foldl' count (gExifKeywords g) (exifKeywords e)
    , gExifLocations = foldl' count (gExifLocations g) (exifLocations e)
    , gExifCameras   = count (gExifCameras g) (exifCamera e)
    , gExifLenses    = count (gExifLenses g) (exifLens e)
    }
    where count m k = M.insertWith (+) k 1 m

instance Semigroup GroupExif where
  g1 <> g2 =
    GroupExif
    { gExifPeople    = gExifPeople    g1 `merge` gExifPeople    g2
    , gExifKeywords  = gExifKeywords  g1 `merge` gExifKeywords  g2
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
                   Bool -- ^ Flip X.
                   Bool -- ^ Flip Y.

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
      pLocations = cfgLocationPrefix config
      pIgnore = cfgIgnorePrefix config
      dropIgnored ks = filter (\k -> not $ pIgnore `T.isPrefixOf` k) ks
      exifPeople      = foldl' (\e ks ->
                                 case ks of
                                   x:p | x == pPeople -> p ++ e
                                   _ -> e) [] rExifHSubjects
      exifKeywords    = foldl' (\e ks ->
                                  case ks of
                                    x:_ | (x /= pLocations && x /= pPeople) ->
                                          dropIgnored ks ++ e
                                    _ -> e) [] rExifHSubjects
      hierLocations   = foldr (\ks e ->
                                  case ks of
                                    x:ls | x == pLocations -> ls ++ e
                                    _ -> e) [] rExifHSubjects
      fieldLocations  = catMaybes [rExifCountry, rExifState,
                                   rExifProvince, rExifCity]
      exifLocations   = nub $ hierLocations ++ fieldLocations
      exifCamera      = fromMaybe unknown rExifCamera
      exifLens        = fromMaybe unknown rExifLens
      exifSerial      = fromMaybe unknown rExifSerial
      exifOrientation = fromMaybe OrientationTopLeft rExifOrientation
      exifCreateDate  = rExifCreateDate
  in Exif{..}

-- | Promotion rules for file to exif
promoteFileExif :: Maybe Exif -> Maybe Exif -> [Exif] -> Maybe Exif
promoteFileExif re se je =
  let summer :: (Exif -> [a]) -> [a]
      summer fn = concat (maybeToList (fn <$> re) ++
                          maybeToList (fn <$> se) ++
                          map fn je)
      first :: (Exif -> Maybe a) -> Maybe a
      first fn = msum $ [re >>= fn, se >>= fn] ++ map fn je
      first' :: (Exif -> a) -> a -> a
      first' fn d = case (catMaybes [fn <$> re, fn <$> se] ++
                                    map fn je) of
                      [] -> d
                      x:_ -> x
      exifPeople'      = summer exifPeople
      exifKeywords'    = summer exifKeywords
      exifLocations'   = summer exifLocations
      exifCamera'      = first' exifCamera unknown
      exifSerial'      = first' exifSerial unknown
      exifLens'        = first' exifLens unknown
      exifOrientation' = first' exifOrientation OrientationTopLeft
      exifCreateDate'  = first  exifCreateDate
  in Just $ Exif { exifPeople      = exifPeople'
                 , exifKeywords    = exifKeywords'
                 , exifLocations   = exifLocations'
                 , exifCamera      = exifCamera'
                 , exifSerial      = exifSerial'
                 , exifLens        = exifLens'
                 , exifOrientation = exifOrientation'
                 , exifCreateDate  = exifCreateDate'
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
  let epath = bExifPath config path
  exists <- fileExist epath
  if exists
    then do
      contents <- BS.readFile epath
      return $ case Data.Store.decode contents of
        Left _ -> Nothing
        Right v -> Just v
    else return Nothing

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
  let epath = exifPath config path
  exists <- fileExist epath
  if exists
    then do
      contents <- BS.readFile epath
      return $ decodeStrict' contents
    else return Nothing

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
                                return $ (M.insert p e c, m)
                 ) (cache1, []) m1
  jsons <- if null m2
             then return []
             else do
               exifs <- extractExifs dir m2
               return $ fromMaybe [] (parseExifs exifs)
  localCache <- foldM (\m r -> do
                          (path, e) <- writeExifs config dir r
                          return $ M.insert path e m
                      ) cache2 jsons
  return localCache

exifPath :: Config -> FilePath -> FilePath
exifPath config path =
  cachedBasename config path ++ "-exif"

bExifPath :: Config -> FilePath -> FilePath
bExifPath config path =
  cachedBasename config path ++ "-bexif" ++ devSuffix

extractExifs :: FilePath -> [FilePath] -> IO BS.ByteString
extractExifs dir paths = do
  let args = [
        "-json",
        "-struct",
        "-n"
        ] ++ paths
      pconfig =
        setStdin closed
        . setCloseFds True
        . setWorkingDir dir
        $ proc "exiftool" args
  (_, out, _) <- readProcess pconfig
  -- Note: exiftool exits with non-zero exit code if errors happened,
  -- but still generates valid JSON output. So we ignore the exit code
  -- completely, as we can't attribute the error to a specific
  -- file. That should be rather done via 'Error' entry in the object.
  return $ BSL.toStrict out

parseExifs :: BS.ByteString -> Maybe [RawExif]
parseExifs = decodeStrict'

-- | Tries to compute the date the image was taken.
parseCreateDate :: Object -> Parser (Maybe LocalTime)
parseCreateDate o = do
  dto  <- msum $ map (o .:?) [ "SubSecDateTimeOriginal"
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
$(makeStore ''TimeOfDay)
$(makeStore ''LocalTime)