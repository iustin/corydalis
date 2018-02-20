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

module Handler.LensStats
  ( getLensStatsR
  ) where

import           Exif
import           Handler.Utils
import           Import
import           Pics

import qualified Data.Map      as Map
import qualified Data.Text     as T

data GraphData a b = GraphData
  { gdName :: Text
  , gdType :: Text
  , gdX    :: [a]
  , gdY    :: [b]
  , gdText :: Maybe [Text]
--  , gdYAxis :: Maybe Text
  , gdMode :: Maybe Text
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
      top10l = buildTopNLenses bylens 30
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
