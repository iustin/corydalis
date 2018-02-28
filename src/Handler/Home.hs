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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoCPP                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home
  ( getHomeR
  ) where

import           Exif
import           Handler.Utils
import           Handler.Widgets
import           Import
import           Indexer
import           Pics

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Text       as Text

getHomeR :: Handler Html
getHomeR = do
  pics <- getPics
  let all_years = Map.foldl' (\s ->
                                maybe s (`Set.insert` s) . pdYear
                             ) Set.empty (repoDirs pics)
      topN n f =
        take n .
        -- sortBy + flip (compare `on`) = sortBy + reverse
        sortBy (flip compare `on` snd) .
        foldl' (\l (a, b) ->
                  case a of
                    Nothing -> l
                    Just a' -> (a', b):l
               ) [] .
        Map.toList .
        f $ gexif
      years = Set.toAscList all_years
      gexif = repoExif pics
      topCountries  = topN 10 gExifCountries
      topProvinces  = topN  7 gExifProvinces
      topCities     = topN  7 gExifCities
      topLocations  = topN  7 gExifLocations
      topPeople     = topN 15 gExifPeople
      topKeywords   = topN 10 gExifKeywords
  defaultLayout $ do
    setHtmlTitle "home"
    $(widgetFile "homepage")
