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

module Handler.LensStats
  ( getLensStatsR
  ) where

import Import
import Pics
import Exif
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Text as T

data GraphData a b = GraphData
  { gdName  :: Text
  , gdType  :: Text
  , gdX     :: [a]
  , gdY     :: [b]
  , gdText  :: Maybe [Text]
--  , gdYAxis :: Maybe Text
  , gdMode  :: Maybe Text
  }

instance Default (GraphData a b) where
  def = GraphData { gdName = ""
                  , gdType = "scatter"
                  , gdX = []
                  , gdY = []
                  , gdText = Nothing
                  , gdMode = Nothing
                  }

instance (ToJSON a, ToJSON b) => ToJSON (GraphData a b) where
  toJSON GraphData {..} =
    object [ "name"  .= gdName
           , "type"  .= gdType
           , "x"     .= gdX
           , "y"     .= gdY
           , "text"  .= gdText
           --, "yaxis" .= gdYAxis
           , "mode"  .= gdMode
           , "marker" .= object [ "size" .= map (const (15::Int)) gdX]
           ]

getLensStatsR :: Handler TypedContent
getLensStatsR = do
  pics <- getPics
  let RepoStats
        (Stats _ _ _ _ _ _ _ _ _ _ _ bylens) _ =
          repoStats pics
      lenses = Map.toList bylens
      buildTop10 m n = let allItems = sortBy (flip compare) $
                             Map.foldlWithKey' (\a k (li, (cnt, sz)) ->
                                                  (cnt, sz, k, li):a) [] m
                           top10 = if length allItems > n
                                     then let t10 = reverse $ take (n-1) allItems
                                              r  = drop (n-1) allItems
                                              (rc, rs) = foldl' (\(c, s) (cnt, sz, _, _) ->
                                                                   (c+cnt, s+sz)) (0, 0) r
                                          in (rc, rs, "Others", unknownLens): t10
                                     else allItems
                       in top10
      top10l = buildTop10 bylens 30
      bestName li = if T.length (liName li) < T.length (liSpec li)
                       then liName li
                       else liSpec li
      jsonl = foldl' (\a (cnt, _, k, li) ->
                        def { gdName = bestName li
                            , gdType = "bar"
                            , gdMode = Just "markers"
                            , gdX = [k]
                            , gdY = [fromIntegral cnt]
                            }:a)
              ([]::[GraphData Text Int64]) top10l
  let html = do
        setTitle "Corydalis: lens statistics"
        addScript $ StaticR js_plotly_js
        $(widgetFile "lensstats")
  defaultLayoutJson html (return $ object [ "lenses"  .= jsonl
                                          ])
