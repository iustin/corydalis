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
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Indexer ( Atom(..)
               , AtomType(..)
               , atomNames
               , atomName
               , buildAtom
               , buildSearchFunction
               ) where

import Exif
import Pics

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Read (readMaybe)

data AtomType = TLocation
              | TPerson
              | TKeyword
              | TYear
                deriving (Enum, Bounded)

data Atom = Location Text
          | Person   Text
          | Keyword  Text
          | Year     Integer

atomNames :: [(AtomType, Text)]
atomNames = map (\t -> (t, atomName t)) [minBound..maxBound]

atomName :: AtomType -> Text
atomName TLocation = "loc"
atomName TPerson   = "who"
atomName TKeyword  = "kw"
atomName TYear     = "when"

buildAtom :: AtomType -> Text -> Maybe Atom
buildAtom TLocation place = Just $ Location place
buildAtom TPerson who = Just $ Person who
buildAtom TKeyword kw = Just $ Keyword kw
buildAtom TYear when =
  Year <$> readMaybe (Text.unpack when)

buildSearchFunction :: Atom -> (PicDir -> Bool)
buildSearchFunction (Location loc) =
  \f -> loc `Map.member` (gExifLocations $ pdExif f)

buildSearchFunction (Person who) =
  \f -> who `Map.member` (gExifPeople $ pdExif f)

buildSearchFunction (Keyword what) =
  \f -> what `Map.member` (gExifKeywords $ pdExif f)

buildSearchFunction (Year when) =
  \f -> maybe False (== when) (pdYear f)
