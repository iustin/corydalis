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
               , atomTypeDescriptions
               , buildAtom
               , buildSearchFunction
               , getAtoms
               ) where

import qualified Data.Map  as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import           Text.Read (readMaybe)
import           Yesod     (PathPiece (..))

import           Exif
import           Pics

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

buildSearchFunction :: Atom -> (PicDir -> Bool)
buildSearchFunction (Country loc) =
  \f -> loc `Map.member` gExifCountries (pdExif f)

buildSearchFunction (Province loc) =
  \f -> loc `Map.member` gExifProvinces (pdExif f)

buildSearchFunction (City loc) =
  \f -> loc `Map.member` gExifCities (pdExif f)

buildSearchFunction (Location loc) =
  \f -> loc `Map.member` gExifLocations (pdExif f)

buildSearchFunction (Person who) =
  \f -> who `Map.member` gExifPeople (pdExif f)

buildSearchFunction (Keyword what) =
  \f -> what `Map.member` gExifKeywords (pdExif f)

buildSearchFunction (Year year) =
  (== Just year) . pdYear

getAtoms :: AtomType -> Repository -> NameStats
getAtoms TCountry  = gExifCountries . repoExif
getAtoms TProvince = gExifProvinces . repoExif
getAtoms TCity     = gExifCities    . repoExif
getAtoms TLocation = gExifLocations . repoExif
getAtoms TPerson   = gExifPeople    . repoExif
getAtoms TKeyword  = gExifKeywords  . repoExif
getAtoms TYear     = const Map.empty
