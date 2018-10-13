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

module Handler.Curate
  ( getCurateR
  ) where

import qualified Data.Map      as Map

import           Exif          (unknownLens)
import           Handler.Items
import           Handler.Utils
import           Import
import           Indexer
import           Pics
import           Types

data XGraphData a b = XGraphData
  { xgdName :: Text
  , xgdType :: Text
  , xgdX    :: [a]
  , xgdY    :: [b]
  , xgdText :: Maybe [Text]
--  , gdYAxis :: Maybe Text
  , xgdMode :: Maybe Text
  }

instance Default (XGraphData a b) where
  def = XGraphData { xgdName = ""
                   , xgdType = "scatter"
                   , xgdX = []
                   , xgdY = []
                   , xgdText = Nothing
                   , xgdMode = Nothing
                  }

instance (ToJSON a, ToJSON b) => ToJSON (XGraphData a b) where
  toJSON XGraphData {..} =
    object [ "name"  .= xgdName
           , "type"  .= xgdType
           , "x"     .= xgdX
           , "y"     .= xgdY
           , "text"  .= xgdText
           --, "yaxis" .= gdYAxis
           , "mode"  .= xgdMode
           , "marker" .= object [ "size" .= map (const (15::Int)) xgdX]
           ]

getCurateR :: Handler TypedContent
getCurateR = do
  pics <- getPics
  let RepoStats
        (Stats unprocessed standalone processed orphaned untracked movies
             rawsize procsize standalonesize sidecarsize untrksize moviesize
             bycamera bylens) fcm =
          repoStats pics
      allpics = unprocessed + standalone + processed + movies
      totalsize = rawsize + procsize + standalonesize + sidecarsize + untrksize + moviesize
      fstats = Map.toAscList fcm
      numfolders = Map.size $ repoDirs pics
      numLenses = Map.size bylens
      numCameras = Map.size bycamera
      all_fc = [minBound..maxBound]
      buildTop10 m n = let allItems = sortBy (flip compare) $
                             Map.foldlWithKey' (\a k (Occurrence cnt sz _ _) ->
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
                       def { xgdName = k
                           , xgdType = "scatter"
                           , xgdMode = Just "markers"
                           , xgdX = [fromIntegral cnt]
                           , xgdY = [fromIntegral sz]
                           }:a)
               ([]::[XGraphData Int64 Int64]) top10c
      top10l = buildTopNItems unknownLens bylens 12
      jsonl = foldl' (\a (cnt, _, k, _) ->
                        def { xgdName = k
                            , xgdType = "bar"
                            , xgdMode = Just "markers"
                            , xgdX = [k]
                            , xgdY = [fromIntegral cnt]
                            }:a)
              ([]::[XGraphData Text Int64]) top10l
      perFolderStats = Map.foldl'
                       (\l f -> let stats = computeFolderStats f
                                in (fromIntegral $ totalStatsSize stats,
                                    fromIntegral $ totalStatsCount stats,
                                    pdName f):l) [] (repoDirs pics)
      (xdata, ydata, textdata) = unzip3 perFolderStats
      j2 = [ def { xgdName = "Folders"
                 , xgdType = "scatter"
                 , xgdMode = Just "markers"
                 , xgdX = xdata
                 , xgdY = ydata
                 , xgdText = Just textdata
                 }::XGraphData Int64 Int64
           ]
      topN n =
        splitAt n .
        -- sortBy + flip (compare `on`) = sortBy + reverse
        sortBy (flip compare `on` snd) .
        foldl' (\l (a, b) ->
                  case a of
                    Nothing -> l
                    Just a' -> (a', b):l
               ) [] .
        Map.toList
      problems = topN 3 $ repoProblems pics
  let html = do
        setTitle "Corydalis: curate"
        addPlotly
        $(widgetFile "curate")
  defaultLayoutJson html (return $ object [ "global" .= json
                                          , "folders" .= j2
                                          , "lenses"  .= jsonl
                                          ])
