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

{-# LANGUAGE LambdaCase            #-}
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
import           Stats               (keeperRate)

import qualified Data.Map.Strict     as Map
import           Data.Time.Calendar
import           Data.Time.LocalTime

getByCamera :: Repository -> Map SymbolizedItem (Occurrence CameraInfo)
getByCamera = sByCamera . rsPicStats . repoStats

getCameraInfoR :: ShortText -> Handler TypedContent
getCameraInfoR cameraname = do
  cameraSymbol <- lookupSymbolized cameraname >>= \case
                  Nothing -> notFound
                  Just s  -> return s
  pics <- getPics
  let bycamera = getByCamera pics
  camera <- maybe notFound return $ cameraSymbol `Map.lookup` bycamera
  let cameraExif = if cameraname == unknown
                   then Nothing
                   else Just cameraname
      cameraExif' = mkSymbolizedItem <$> cameraExif
      images = filterImagesBy (\i -> (exifCamera . imgExif) i == cameraExif') pics
      lensName = liName . exifLens . imgExif
      lensCounts = countImagesBy (Just . lensName) images
      numLenses = length lensCounts
      imgTopBot = imageDateRange lensName images
      keepR = formatKeeperRate <$> keeperRate camera
      lensapflobj = buildLensApFL images
      -- timeline stats
      camerapicstats = computeImagesStats images
      -- TODO: cleanup desymbolization
      timelineobj = buildCamLensStats lensOthers 30 10 lensShortName (deSymbolizeItem' . liName) (Map.mapKeys deSymbolizeItem' $ sByLens camerapicstats)
      obj = object [ "lensapfl" .= lensapflobj
                   , "trends" .= timelineobj
                   ]
      -- end timeline stats
      html = do
        setTitle "Corydalis: camera information"
        $(widgetFile "camerainfo")
  defaultLayoutJson html (return obj)

getCameraStatsR :: Handler TypedContent
getCameraStatsR = do
  pics <- getPics
  let bycamera = Map.mapKeys deSymbolizeItem' $ getByCamera pics
      cameras = Map.toList bycamera
      others = def { ciName = mkSymbolizedItem ("others"::Text) }
      desymmer = deSymbolizeItem' . ciName
      obj = buildCamLensStats others 30 10 desymmer desymmer bycamera
  let html = do
        setTitle "Corydalis: camera statistics"
        $(widgetFile "camerastats")
  defaultLayoutJson html (return obj)
