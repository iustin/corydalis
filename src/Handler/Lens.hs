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

import qualified Data.Map            as Map
import           Data.Time.Calendar
import           Data.Time.LocalTime

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
                  in fromNullable cds >>= (\nn -> Just (head nn, last nn))
      lensapflobj = buildLensApFL images
      -- timeline stats
      camerapicstats = computeImagesStats images
      others = def { ciName = "others" }
      timelineobj = buildCamLensStats others 30 10 ciName ciName (sByCamera camerapicstats)
      -- end timeline stats
      obj = object [ "lensapfl" .= lensapflobj, "trends" .= timelineobj ]
      html = do
        setTitle "Corydalis: lens information"
        $(widgetFile "lensinfo")
  defaultLayoutJson html (return obj)

getLensStatsR :: Handler TypedContent
getLensStatsR = do
  pics <- getPics
  let bylens = getByLens pics
      lenses = Map.toList bylens
      others = unknownLens { liName = "others" }
      obj = buildCamLensStats others 30 10 lensShortName liName bylens
  let html = do
        setTitle "Corydalis: lens statistics"
        $(widgetFile "lensstats")
  defaultLayoutJson html (return obj)
