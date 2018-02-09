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

module Handler.Home ( getCurateR
                    , getHomeR
                    ) where

import Import
import Exif
import Pics
import Types
import Indexer
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
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

getCurateR :: Handler TypedContent
getCurateR = do
  pics <- getPics
  let RepoStats
        (Stats unprocessed standalone processed orphaned untracked
             rawsize procsize standalonesize sidecarsize untrksize
             bycamera bylens) fcm =
          repoStats pics
      allpics = unprocessed + standalone + processed
      fstats = Map.toAscList fcm
      numfolders = Map.size $ repoDirs pics
      all_fc = [minBound..maxBound]
      buildTop10 m n = let allItems = sortBy (flip compare) $
                             Map.foldlWithKey' (\a k (cnt, sz) ->
                                                  (cnt, sz, k):a) [] m
                           top10 = if length allItems > n
                                     then let t10 = reverse $ take (n-1) allItems
                                              r  = drop (n-1) allItems
                                              (rc, rs) = foldl' (\(c, s) (cnt, sz, _) ->
                                                                   (c+cnt, s+sz)) (0, 0) r
                                          in (rc, rs, "Others"): t10
                                     else allItems
                       in top10
      top10c = buildTop10 bycamera 10
      json = foldl' (\a (cnt, sz, k) ->
                       def { gdName = k
                           , gdType = "scatter"
                           , gdMode = Just "markers"
                           , gdX = [fromIntegral cnt]
                           , gdY = [fromIntegral sz]
                           }:a)
               ([]::[GraphData Int64 Int64]) top10c
      top10l = buildTop10 bylens 12
      jsonl = foldl' (\a (cnt, _, k) ->
                        def { gdName = k
                            , gdType = "bar"
                            , gdMode = Just "markers"
                            , gdX = [k]
                            , gdY = [fromIntegral cnt]
                            }:a)
              ([]::[GraphData Text Int64]) top10l
      perFolderStats = Map.foldl'
                       (\l f -> let stats = computeFolderStats f
                                in (fromIntegral $ totalStatsSize stats,
                                    fromIntegral $ totalStatsCount stats,
                                    pdName f):l) [] (repoDirs pics)
      (xdata, ydata, textdata) = unzip3 perFolderStats
      j2 = [ def { gdName = "Folders"
                 , gdType = "scatter"
                 , gdMode = Just "markers"
                 , gdX = xdata
                 , gdY = ydata
                 , gdText = Just textdata
                 }::GraphData Int64 Int64
           ]
  let html = do
        setTitle "Corydalis: curate"
        addScript $ StaticR js_plotly_js
        $(widgetFile "curate")
  defaultLayoutJson html (return $ object [ "global" .= json
                                          , "folders" .= j2
                                          , "lenses"  .= jsonl
                                          ])
