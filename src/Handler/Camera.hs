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

getCameraInfoR :: Text -> Handler TypedContent
getCameraInfoR cameraname = do
  pics <- getPics
  let bycamera = getByCamera pics
  camera <- maybe notFound return $ cameraname `Map.lookup` bycamera
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
        $(widgetFile "camerainfo")
        addScript $ StaticR corydalis_js_camerainfo_js
  defaultLayoutJson html (return obj)

getCameraStatsR :: Handler TypedContent
getCameraStatsR = do
  pics <- getPics
  let bycamera = getByCamera pics
      cameras = Map.toList bycamera
      others = def { ciName = "others" }
      obj = buildCamLensStats others 30 10 ciName ciName bycamera
  let html = do
        setTitle "Corydalis: camera statistics"
        $(widgetFile "camerastats")
        addScript $ StaticR corydalis_js_camerastats_js
  defaultLayoutJson html (return obj)
