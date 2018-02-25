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

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ViewPatterns       #-}

module Indexer ( Symbol(..)
               , Atom(..)
               , symbolNames
               , symbolName
               , negSymbolName
               , atomTypeDescriptions
               , atomDescription
               , parseAtom
               , buildMissingAtom
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
import           Data.Maybe                 (isNothing)
import           Data.Semigroup             ((<>))
import           Data.Set                   (Set)
import qualified Data.Set                   as Set
import           Data.String                (IsString)
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Read             as Text
import           Yesod                      (PathPiece (..))

import           Exif
import           Pics
import           Types                      (UrlParams)

data Symbol = TCountry
              | TProvince
              | TCity
              | TLocation
              | TPerson
              | TKeyword
              | TYear
              | TCamera
                deriving (Enum, Bounded, Show, Read, Eq)

instance PathPiece Symbol where
  toPathPiece  = atomTypeDescriptions
  fromPathPiece "countries" = Just TCountry
  fromPathPiece "provinces" = Just TProvince
  fromPathPiece "cities"    = Just TCity
  fromPathPiece "locations" = Just TLocation
  fromPathPiece "people"    = Just TPerson
  fromPathPiece "keywords"  = Just TKeyword
  fromPathPiece "years"     = Just TYear
  fromPathPiece "cameras"   = Just TCamera
  fromPathPiece _           = Nothing

data Op a where
  OpEqual   :: (Eq a) => a -> Op a
  OpFuzzy   :: Text -> Op Text
  OpMissing :: Op a
  OpLt      :: (Num a, Ord a) => a -> Op a
  OpGt      :: (Num a, Ord a) => a -> Op a

deriving instance Show a => Show (Op a)

fuzzyMatch :: Text -> Text -> Bool
fuzzyMatch fz =
  (Text.toCaseFold fz `Text.isInfixOf`) . Text.toCaseFold

eval :: Op a -> Maybe a -> Bool
eval (OpEqual a) = (Just a ==)
eval (OpFuzzy a) = maybe False (fuzzyMatch a)
eval  OpMissing  = isNothing
eval (OpLt    a) = maybe False (< a)
eval (OpGt    a) = maybe False (> a)

data Atom = Country  (Op Text)
          | Province (Op Text)
          | City     (Op Text)
          | Location (Op Text)
          | Person   (Op Text)
          | Keyword  (Op Text)
          | Year     (Op Integer)
          | Camera   (Op Text)
          | And Atom Atom
          | Or  Atom Atom
          | Not Atom
          | All [Atom]
          | Any [Atom]
          deriving (Show)

symbolNames :: [(Symbol, Text)]
symbolNames = map (\t -> (t, symbolName t)) [minBound..maxBound]

symbolName :: Symbol -> Text
symbolName TCountry  = "country"
symbolName TProvince = "province"
symbolName TCity     = "city"
symbolName TLocation = "location"
symbolName TPerson   = "person"
symbolName TKeyword  = "keyword"
symbolName TYear     = "year"
symbolName TCamera   = "camera"

negSymbolName :: Symbol -> Text
negSymbolName atom = "no-" <> symbolName atom

parseSymbol :: Text -> Maybe Symbol
parseSymbol "country"  = Just TCountry
parseSymbol "province" = Just TProvince
parseSymbol "city"     = Just TCity
parseSymbol "location" = Just TLocation
parseSymbol "person"   = Just TPerson
parseSymbol "keyword"  = Just TKeyword
parseSymbol "year"     = Just TYear
parseSymbol "camera"   = Just TCamera
parseSymbol _          = Nothing

buildMissingAtom :: Symbol -> Atom
buildMissingAtom s =
  case s of
    TCountry  -> Country   OpMissing
    TProvince -> Province  OpMissing
    TCity     -> City      OpMissing
    TLocation -> Location  OpMissing
    TPerson   -> Person    OpMissing
    TKeyword  -> Keyword   OpMissing
    TYear     -> Year      OpMissing
    TCamera   -> Camera    OpMissing

parseAtom :: Text -> Text -> Maybe Atom
parseAtom (Text.splitAt 3 -> ("no-", v)) _ =
  buildMissingAtom <$> parseSymbol v

parseAtom a v = do
  s <- parseSymbol a
  let dec = parseDecimal v
      str = parseString v
  case s of
    TCountry  -> str >>= Just . Country
    TProvince -> str >>= Just . Province
    TCity     -> str >>= Just . City
    TLocation -> str >>= Just . Location
    TPerson   -> str >>= Just . Person
    TKeyword  -> str >>= Just . Keyword
    TYear     -> dec >>= Just . Year
    TCamera   -> str >>= Just . Camera

atomTypeDescriptions :: Symbol -> Text
atomTypeDescriptions TCountry  = "countries"
atomTypeDescriptions TProvince = "provinces"
atomTypeDescriptions TCity     = "cities"
atomTypeDescriptions TLocation = "locations"
atomTypeDescriptions TPerson   = "people"
atomTypeDescriptions TKeyword  = "keywords"
atomTypeDescriptions TYear     = "years"
atomTypeDescriptions TCamera   = "cameras"

class (Show a, IsString a) => ToText a where
  toText :: a -> Text
  toText = Text.pack . show

instance ToText Text where
  toText = id

-- | Describe a value.
--
-- 'describe' will show either lack of information about subject, or
-- tagged with empty value, or the actual value.
describe :: (ToText a) => Text -> Op a -> Text
describe a (OpEqual "") = a <> " is empty"
describe a (OpEqual v)  = a <> " is '" <> toText v <> "'"
describe a (OpFuzzy v)  = a <> " contains " <> v
describe a  OpMissing   = "has no " <> a <> " information"
describe a (OpLt v)     = a <> " is smaller than " <> toText v
describe a (OpGt v)     = a <> " is greater than " <> toText v

atomDescription :: Atom -> Text
atomDescription (Country  place) = describe "country"  place
atomDescription (Province place) = describe "province" place
atomDescription (City     place) = describe "city"     place
atomDescription (Location place) = describe "location" place
atomDescription (Person   (OpEqual who)) =
  case who of
    "" -> "has an empty person tag"
    p  -> formatPerson False p <> " is in the picture"
atomDescription (Person (OpFuzzy v)) =
  "tagged with a person with a name containing " <> v
atomDescription (Person OpMissing) = "has no person information"
atomDescription (Keyword (OpEqual keyword)) =
  case keyword of
    "" -> "tagged with an empty keyword"
    k  -> "tagged with keyword " <> k <> ""
atomDescription (Keyword (OpFuzzy v)) =
  "tagged with a keyword containing " <> v
atomDescription (Keyword OpMissing) = "not tagged with any keywords"
atomDescription (Year (OpEqual year)) = "taken in the year " <> Text.pack (show year)
atomDescription (Camera OpMissing) = "has no camera information"
atomDescription (Camera (OpEqual "")) = "has defined but empty camera information"
atomDescription (Camera (OpEqual v)) = "shot with a " <> v <> " camera"
atomDescription (Camera (OpFuzzy v)) =
  "shot with a camera named like " <> v
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

-- | Set search function for either membership or null set checking.
setSearch :: Op Text -> Set Text -> Bool
setSearch OpMissing = Set.null
setSearch (OpEqual v) = (v `Set.member`)
setSearch (OpFuzzy f)=
  Set.foldr' (\a v -> v || fuzzyMatch f a) False

folderSearchFunction :: Atom -> PicDir -> Bool
folderSearchFunction a =
  any (imageSearchFunction a) . pdImages

imageSearchFunction :: Atom -> (Image -> Bool)
imageSearchFunction (Country loc) =
  eval loc . exifCountry . imgExif

imageSearchFunction (Province loc) =
  eval loc . exifProvince . imgExif

imageSearchFunction (City loc) =
  eval loc . exifCity . imgExif

imageSearchFunction (Location loc) =
  eval loc . exifLocation . imgExif

imageSearchFunction (Person who) =
  setSearch who . exifPeople . imgExif

imageSearchFunction (Keyword keyword) =
  setSearch keyword . exifKeywords . imgExif

imageSearchFunction (Year year) =
  eval year . imageYear

imageSearchFunction (Camera camera) =
  eval camera . exifCamera . imgExif

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

getAtoms :: Symbol -> Repository -> NameStats
getAtoms TCountry  = gExifCountries . repoExif
getAtoms TProvince = gExifProvinces . repoExif
getAtoms TCity     = gExifCities    . repoExif
getAtoms TLocation = gExifLocations . repoExif
getAtoms TPerson   = gExifPeople    . repoExif
getAtoms TKeyword  = gExifKeywords  . repoExif
getAtoms TCamera   = gExifCameras   . repoExif
getAtoms TYear     = const Map.empty

-- | Builder for all atom.
--
-- Builds an 'All' atom, with special casing for small values.
allAtom :: [Atom] -> Atom
allAtom [x]    = x       -- no sense in over-wrapping.
allAtom [x, y] = And x y -- 'x and y' is better than 'all of [x, y]'.
allAtom xs     = All xs  -- and normal wrapping.

-- | Builder for any atom.
-- Builds an 'Any' atom, with special casing for small values.
anyAtom :: [Atom] -> Atom
anyAtom [x]    = x
anyAtom [x, y] = Or x y
anyAtom xs     = Any xs

rpnParser :: [Atom] -> (Text, Text) -> Except Text [Atom]
rpnParser (x:y:ys) ("and",_) = return $ And x y:ys
rpnParser (x:y:ys) ("or",_) = return $ Or x y:ys
rpnParser (x:xs) ("not", _) =
  let a = case x of
            Not y -> y
            _     -> Not x
  in return $ a:xs
rpnParser xs ("all", _) = return [allAtom xs]
rpnParser xs ("any", _) = return [anyAtom xs]
rpnParser xs (an, av) =
  let v = parseAtom an av
  in case v of
    Just v' -> return $ v':xs
    Nothing -> throwE $ "Failed to parse the atom " <>
               an <> "=" <> av <>
               " with stack " <> Text.pack (show xs)

parseString :: Text -> Maybe (Op Text)
parseString (Text.uncons -> Just ('~', v)) = Just $ OpFuzzy v
parseString v                              = Just $ OpEqual v

parseDecimal :: (Integral a, Ord a) => Text -> Maybe (Op a)
parseDecimal v =
  case Text.uncons v of
    Just ('<', v') -> OpLt    <$> go v'
    Just ('>', v') -> OpGt    <$> go v'
    _              -> OpEqual <$> go v
  where go w =
          case Text.decimal w of
            Right (w',"") -> Just w'
            _             -> Nothing

parseAtomParams :: [(Text, Text)] -> Either Text Atom
parseAtomParams params = runExcept $
  allAtom <$> foldM rpnParser [] params

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
