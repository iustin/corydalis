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

module Handler.Lens
  ( getLensInfoR
  , getLensStatsR
  ) where

import           Exif
import           Handler.Utils
import           Import
import           Indexer
import           Pics

import qualified Data.Map      as Map

getByLens :: Repository -> Map Text (Occurrence LensInfo)
getByLens = sByLens . rsPicStats . repoStats

getLensInfoR :: Text -> Handler TypedContent
getLensInfoR lensname = do
  pics <- getPics
  let bylens = getByLens pics
  lens <- case lensname `Map.lookup` bylens of
            Nothing  -> notFound
            Just occ -> return $ ocData occ
  let images = filterImagesBy (\i -> (liName . exifLens . imgExif) i == lensname) pics
      cameras = foldl' (\m i -> case exifCamera (imgExif i) of
                                  Nothing -> m
                                  Just c  ->  Map.insertWith (+) c counterOne m) Map.empty images
      cameraCounts =
        sort . map (\(a, b) -> (b, a)) . Map.toList $ cameras
      topCamera = listToMaybe $ reverse cameraCounts
      botCamera = listToMaybe cameraCounts
      numCameras = Map.size cameras
      imgTopBot = let cds =
                        sort .
                        foldl' (\a i -> let e = imgExif i
                                            cd = exifCreateDate e
                                            cam = fromMaybe unknown $ exifCamera e
                                        in case cd of
                                             Nothing  -> a
                                             Just cd' -> (cd', cam):a) [] $ images
                  in maybe Nothing (\nn -> Just (head nn, last nn)) $ fromNullable cds
      obj = buildLensApFL images
      html = do
        setTitle "Corydalis: lens information"
        addPlotly
        $(widgetFile "lensinfo")
  defaultLayoutJson html (return obj)

getLensStatsR :: Handler TypedContent
getLensStatsR = do
  pics <- getPics
  let bylens = getByLens pics
      lenses = Map.toList bylens
      top10l = buildTopNItems unknownLens bylens 30
      jsonl = foldl' (\a (cnt, _, k, li) ->
                        def { gdName = lensShortName li
                            , gdType = "bar"
                            , gdMode = Just "markers"
                            , gdX = Just [k]
                            , gdY = Just [fromIntegral cnt]
                            }:a)
              ([]::[GraphData Text Int64 Int64]) top10l
  let html = do
        setTitle "Corydalis: lens statistics"
        addPlotly
        $(widgetFile "lensstats")
  defaultLayoutJson html (return $ object [ "lenses"  .= jsonl
                                          ])
