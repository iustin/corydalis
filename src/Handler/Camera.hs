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
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoCPP                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Camera
  ( getCameraInfoR
  , getCameraStatsR
  ) where

import           Exif
import           Handler.Utils
import           Import
import           Indexer
import           Pics

import qualified Data.Map            as Map
import qualified Data.Text.Lazy      as LT
import           Data.Time.Calendar
import           Data.Time.LocalTime
import           Formatting

getByCamera :: Repository -> Map Text (Occurrence CameraInfo)
getByCamera = sByCamera . rsPicStats . repoStats

keeperRate :: Occurrence CameraInfo -> Maybe Double
keeperRate Occurrence{..} =
  case ciShutterCount ocData of
    Nothing -> Nothing
    Just (scMin, scMax) ->
      let numImgs = fromIntegral ocFiles
          delta = fromIntegral $ scMax - scMin + 1
      in Just $ numImgs / delta

formatKeeperRate :: Double -> LT.Text
formatKeeperRate = format (fixed 2 % "%") . (* 100)

formatDate :: Integer -> LT.Text
formatDate d =
  let (years, d1) = d `quotRem` 365
      (months, d2) = d1 `quotRem` 30
      (weeks, days) = d2 `quotRem` 7
      pl :: Integer -> LT.Text
      pl n = if n > 1 then "s" else ""
      fpl :: Integer -> LT.Text -> Maybe LT.Text
      fpl n t = if n > 0
                then Just (format (int % " " % text % text) n t (pl n))
                else Nothing
      elems = [ fpl years "year"
              , fpl months "month"
              , fpl weeks "week"
              , fpl days "day"
              ]
  in LT.intercalate " and " $ take 2 $ catMaybes elems

getCameraInfoR :: Text -> Handler TypedContent
getCameraInfoR cameraname = do
  pics <- getPics
  let bycamera = getByCamera pics
  camera <- case cameraname `Map.lookup` bycamera of
              Nothing -> notFound
              Just c  -> return c
  let cameraExif = if cameraname == unknown
                   then Nothing
                   else Just cameraname
      images = filterImagesBy (\i -> (exifCamera . imgExif) i == cameraExif) pics
      lenses = foldl' (\m i -> Map.insertWith (+)
                               (liName . exifLens . imgExif $ i)
                               counterOne m) Map.empty images
      lensCounts =
        sort . map (\(a, b) -> (b, a)) . Map.toList $ lenses
      topLens = listToMaybe $ reverse lensCounts
      botLens = listToMaybe lensCounts
      numLenses = Map.size lenses
      imgTopBot = let cds =
                        sort .
                        foldl' (\a i -> let e = imgExif i
                                            cd = exifCreateDate e
                                            lens = liName $ exifLens e
                                        in case cd of
                                             Nothing  -> a
                                             Just cd' -> (cd', lens):a) [] $ images
                  in maybe Nothing (\nn -> Just (head nn, last nn)) $ fromNullable cds
      keepR = formatKeeperRate <$> keeperRate camera
      obj = buildLensApFL images
      html = do
        setTitle "Corydalis: camera information"
        addPlotly
        $(widgetFile "camerainfo")
  defaultLayoutJson html (return obj)

getCameraStatsR :: Handler TypedContent
getCameraStatsR = do
  pics <- getPics
  let bycamera = getByCamera pics
      cameras = Map.toList bycamera
      top10 = buildTopNItems def bycamera 10
      top30 = buildTopNItems def bycamera 30
      jsonl = foldl' (\a (cnt, _, k, cam, _) ->
                        def { gdName = ciName cam
                            , gdType = "bar"
                            , gdMode = Just "markers"
                            , gdX = Just [k]
                            , gdY = Just [fromIntegral cnt]
                            }:a)
              ([]::[GraphData Text Int64 Int64]) top30
      jsont = foldl' (\a (_, _, _, cam, tr) ->
                        let (d, c) = unzip $ Map.assocs tr
                            d' = map (\(y, m) -> show y ++ "-" ++ show m) d
                            c' = map fromIntegral c
                        in
                        def { gdName = ciName cam
                            , gdType = "scatter"
                            , gdMode = Just "lines+markers"
                            , gdX = Just d'
                            , gdY = Just c'
                            , gdExtra = [("connectgaps", toJSON False)]
                            }:a)
              ([]::[GraphData String Int64 Int64]) top10
  let html = do
        setTitle "Corydalis: camera statistics"
        addPlotly
        $(widgetFile "camerastats")
  defaultLayoutJson html (return $ object [ "cameras"  .= jsonl
                                          , "trends"   .= jsont
                                          ])
