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

module Indexer ( AtomType(..)
               , Atom(..)
               , atomNames
               , atomName
               , parseAName
               , atomTypeDescriptions
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

data Atom = Country  Text
          | Province Text
          | City     Text
          | Location Text
          | Person   Text
          | Keyword  Text
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

parseAName :: Text -> Maybe AtomType
parseAName "country"  = Just TCountry
parseAName "province" = Just TProvince
parseAName "city"     = Just TCity
parseAName "location" = Just TLocation
parseAName "person"   = Just TPerson
parseAName "keyword"  = Just TKeyword
parseAName "year"     = Just TYear
parseAName _          = Nothing

atomTypeDescriptions :: AtomType -> Text
atomTypeDescriptions TCountry  = "countries"
atomTypeDescriptions TProvince = "provinces"
atomTypeDescriptions TCity     = "cities"
atomTypeDescriptions TLocation = "locations"
atomTypeDescriptions TPerson   = "people"
atomTypeDescriptions TKeyword  = "keywords"
atomTypeDescriptions TYear     = "years"

buildAtom :: AtomType -> Text -> Maybe Atom
buildAtom TCountry  place = Just $ Country  place
buildAtom TProvince place = Just $ Province place
buildAtom TCity     place = Just $ City     place
buildAtom TLocation place = Just $ Location place
buildAtom TPerson who = Just $ Person who
buildAtom TKeyword kw = Just $ Keyword kw
buildAtom TYear when =
  Year <$> readMaybe (Text.unpack when)

folderSearchFunction :: Atom -> PicDir -> Bool
folderSearchFunction a =
  any (imageSearchFunction a) . pdImages

imageSearchFunction :: Atom -> (Image -> Bool)
imageSearchFunction (Country loc) =
  (== Just loc) . exifCountry . imgExif

imageSearchFunction (Province loc) =
  (== Just loc) . exifProvince . imgExif

imageSearchFunction (City loc) =
  (== Just loc) . exifCity . imgExif

imageSearchFunction (Location loc) =
  (== Just loc) . exifLocation . imgExif

imageSearchFunction (Person who) =
  (who `Set.member`) . exifPeople . imgExif

imageSearchFunction (Keyword what) =
  (what `Set.member`) . exifKeywords . imgExif

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
  let v = parseAName an >>=
          \at -> buildAtom at av
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
