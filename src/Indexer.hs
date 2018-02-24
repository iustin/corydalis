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

{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Indexer ( AtomType(..)
               , Atom(..)
               , atomNames
               , atomName
               , parseAName
               , atomTypeDescriptions
               , atomDescription
               , buildAtom
               , folderSearchFunction
               , imageSearchFunction
               , getAtoms
               , parseAtomParams
               , buildImageMap
               , searchImages
               ) where

import           Control.Monad              (foldM)
import           Control.Monad.Trans.Except
import           Data.List                  (foldl')
import qualified Data.Map                   as Map
import           Data.Semigroup             ((<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import           Text.Read                  (readMaybe)
import           Yesod                      (PathPiece (..))

import           Exif
import           Pics
import           Types                      (UrlParams)

data AtomType = TCountry
              | TProvince
              | TCity
              | TLocation
              | TPerson
              | TKeyword
              | TYear
                deriving (Enum, Bounded, Show, Read, Eq)

instance PathPiece AtomType where
  toPathPiece  = atomTypeDescriptions
  fromPathPiece "countries" = Just TCountry
  fromPathPiece "provinces" = Just TProvince
  fromPathPiece "cities"    = Just TCity
  fromPathPiece "locations" = Just TLocation
  fromPathPiece "people"    = Just TPerson
  fromPathPiece "keywords"  = Just TKeyword
  fromPathPiece "years"     = Just TYear
  fromPathPiece _           = Nothing

data Atom = Country  (Maybe Text)
          | Province (Maybe Text)
          | City     (Maybe Text)
          | Location (Maybe Text)
          | Person   (Maybe Text)
          | Keyword  (Maybe Text)
          | Year     Integer
          | And Atom Atom
          | Or  Atom Atom
          | Not Atom
          | All [Atom]
          | Any [Atom]
          deriving (Show)

atomNames :: [(AtomType, Text)]
atomNames = map (\t -> (t, atomName t)) [minBound..maxBound]

atomName :: AtomType -> Text
atomName TCountry  = "country"
atomName TProvince = "province"
atomName TCity     = "city"
atomName TLocation = "location"
atomName TPerson   = "person"
atomName TKeyword  = "keyword"
atomName TYear     = "year"

parseAName :: Text -> Text -> Maybe (AtomType, Maybe Text)
parseAName a v = go True a where
  v' x = if x then Just v else Nothing
  go :: Bool -> Text -> Maybe (AtomType, Maybe Text)
  go p "country"  = Just (TCountry,  v' p)
  go p "province" = Just (TProvince, v' p)
  go p "city"     = Just (TCity,     v' p)
  go p "location" = Just (TLocation, v' p)
  go p "person"   = Just (TPerson,   v' p)
  go p "keyword"  = Just (TKeyword,  v' p)
  go p "year"     = Just (TYear,     v' p)
  go False _      = Nothing
  go True (Text.stripPrefix "no-" -> Just suf) =
    go False suf
  go True _ = Nothing

atomTypeDescriptions :: AtomType -> Text
atomTypeDescriptions TCountry  = "countries"
atomTypeDescriptions TProvince = "provinces"
atomTypeDescriptions TCity     = "cities"
atomTypeDescriptions TLocation = "locations"
atomTypeDescriptions TPerson   = "people"
atomTypeDescriptions TKeyword  = "keywords"
atomTypeDescriptions TYear     = "years"

-- | Describe a value.
--
-- 'describe' will show either lack of information about subject, or
-- tagged with empty value, or the actual value.
describe :: Text -> Maybe Text -> Text
describe a Nothing   = "has no " <> a <> " information"
describe a (Just "") = a <> " is empty"
describe a (Just v)  = a <> " is '" <> v <> "'"

atomDescription :: Atom -> Text
atomDescription (Country  place) = describe "country"  place
atomDescription (Province place) = describe "province" place
atomDescription (City     place) = describe "city"     place
atomDescription (Location place) = describe "location" place
atomDescription (Person who) =
  case who of
    Nothing -> "picture has no person information"
    Just "" -> "picture has an empty person tag"
    Just p  -> formatPerson False p <> " is in the picture"
atomDescription (Keyword keyword) =
  case keyword of
    Nothing -> "picture not tagged with any keywords"
    Just "" -> "tagged with an empty keyword"
    Just k  -> "tagged with keyword " <> k <> ""
atomDescription (Year year) = "taken in the year " <> Text.pack (show year)
atomDescription (And a b) =
  mconcat [ "("
          , atomDescription a
          , " and "
          , atomDescription b
          , ")"
          ]

atomDescription (Or a b) =
  mconcat [ "("
          , atomDescription a
          , " or "
          , atomDescription b
          , ")"
          ]
atomDescription (Not a) = "(not " <> atomDescription a <> ")"

atomDescription (All as) = "(all of: " <> Text.intercalate ", " (map atomDescription as) <> ")"
atomDescription (Any as) = "(any of: " <> Text.intercalate ", " (map atomDescription as) <> ")"

buildAtom :: AtomType -> Maybe Text -> Maybe Atom
buildAtom TCountry  place = Just $ Country  place
buildAtom TProvince place = Just $ Province place
buildAtom TCity     place = Just $ City     place
buildAtom TLocation place = Just $ Location place
buildAtom TPerson who = Just $ Person who
buildAtom TKeyword kw = Just $ Keyword kw
buildAtom TYear (Just when) =
  Year <$> readMaybe (Text.unpack when)
buildAtom TYear Nothing = Nothing

-- | Set search function for either membership or null set checking.
setSearch :: Maybe Text -> Set Text -> Bool
setSearch Nothing  = Set.null
setSearch (Just v) = (v `Set.member`)

folderSearchFunction :: Atom -> PicDir -> Bool
folderSearchFunction a =
  any (imageSearchFunction a) . pdImages

imageSearchFunction :: Atom -> (Image -> Bool)
imageSearchFunction (Country loc) =
  (== loc) . exifCountry . imgExif

imageSearchFunction (Province loc) =
  (== loc) . exifProvince . imgExif

imageSearchFunction (City loc) =
  (== loc) . exifCity . imgExif

imageSearchFunction (Location loc) =
  (== loc) . exifLocation . imgExif

imageSearchFunction (Person who) =
  setSearch who . exifPeople . imgExif

imageSearchFunction (Keyword keyword) =
  setSearch keyword . exifKeywords . imgExif

imageSearchFunction (Year year) =
  (== Just year) . imageYear

imageSearchFunction (And a b) = \img ->
  imageSearchFunction a img &&
  imageSearchFunction b img

imageSearchFunction (Or a b) = \img ->
  imageSearchFunction a img ||
  imageSearchFunction b img

imageSearchFunction (Not a) =
  not . imageSearchFunction a

imageSearchFunction (All as) = \img ->
  all (`imageSearchFunction` img) as

imageSearchFunction (Any as) = \img ->
  any (`imageSearchFunction` img) as

getAtoms :: AtomType -> Repository -> NameStats
getAtoms TCountry  = gExifCountries . repoExif
getAtoms TProvince = gExifProvinces . repoExif
getAtoms TCity     = gExifCities    . repoExif
getAtoms TLocation = gExifLocations . repoExif
getAtoms TPerson   = gExifPeople    . repoExif
getAtoms TKeyword  = gExifKeywords  . repoExif
getAtoms TYear     = const Map.empty

rpnParser :: [Atom] -> (Text, Text) -> Except Text [Atom]
rpnParser (x:y:ys) ("and",_) = return $ And x y:ys
rpnParser (x:y:ys) ("or",_) = return $ Or x y:ys
rpnParser (x:xs) ("not", _) =
  let a = case x of
            Not y -> y
            _     -> Not x
  in return $ a:xs
rpnParser xs ("all", _) = return [All xs]
rpnParser xs ("any", _) = return [Any xs]
rpnParser xs (an, av) =
  let v = parseAName an av >>=
          \(at, av') -> buildAtom at av'
  in case v of
    Just v' -> return $ v':xs
    Nothing -> throwE $ "Failed to parse the atom " <>
               an <> "=" <> av <>
               " with stack " <> Text.pack (show xs)

parseAtomParams :: [(Text, Text)] -> Either Text Atom
parseAtomParams params = runExcept $ do
  atoms <- foldM rpnParser [] params
  return $ case atoms of
    [x] ->  x
    -- Note to self : [] must return All [], since Any [] will never
    -- match. So just let it fall through the "other" case.
    xs  -> All xs

-- | Build image map (with static sorting).
buildImageMap :: Atom -> Repository -> SearchResults
buildImageMap atom =
  foldl' (\m img ->
             Map.insert (imgParent img, imgName img) img m
         ) Map.empty .
  filterImagesBy (imageSearchFunction atom)

searchImages :: UrlParams -> Atom -> Repository -> IO SearchResults
searchImages params atom pics = do
  -- Note: the call to buildImageMap must *not* be evaluated,
  -- otherwise we don't gain anything from caching it.
  let lazyimages = buildImageMap atom pics
  getSearchResults lazyimages params
