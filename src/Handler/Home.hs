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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoCPP #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Home
  ( getHomeR
  ) where

import Import
import Exif
import Pics
import Indexer
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T

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
        Map.toList .
        f $ gexif
      years = Set.toAscList all_years
      gexif = repoExif pics
      topPlaces = topN 15 gExifLocations
      topPeople = topN 25 gExifPeople
      topKeywords = topN 10 gExifKeywords
  defaultLayout $ do
    setTitle . toHtml $ ("Corydalis: home"::T.Text)
    $(widgetFile "homepage")
