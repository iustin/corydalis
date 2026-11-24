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

import qualified Data.Map.Strict     as Map
import           Data.Time.Calendar
import           Data.Time.LocalTime

getByLens :: Repository -> Map SymbolizedItem (Occurrence LensInfo)
getByLens = sByLens . rsPicStats . repoStats

getLensInfoR :: ShortText -> Handler TypedContent
getLensInfoR lensname = do
  pics <- getPics
  lensSymbol <- lookupSymbolized lensname >>= \case
                  Nothing -> notFound
                  Just s  -> return s
  let bylens = getByLens pics
  lens <- case lensSymbol `Map.lookup` bylens of
            Nothing  -> notFound
            Just occ -> return $ ocData occ
  let images = filterImagesBy (\i -> (liName . exifLens . imgExif) i == lensSymbol) pics
      cameraCounts =
        countImagesBy (fmap (deSymbolizeItem' :: SymbolizedItem -> ShortText) .
                       exifCamera . imgExif) images
      numCameras = length cameraCounts
      imgTopBot = imageDateRange (fromMaybe unknown . maybeDesymbolizeItem' .
                                  exifCamera . imgExif) images
      lensapflobj = buildLensApFL images
      -- timeline stats
      camerapicstats = computeImagesStats images
      others = def { ciName = mkSymbolizedItem ("others"::Text) }
      desymmer = deSymbolizeItem' . ciName
      timelineobj = buildCamLensStats others 30 10 desymmer desymmer (Map.mapKeys deSymbolizeItem' $ sByCamera camerapicstats)
      -- end timeline stats
      obj = object [ "lensapfl" .= lensapflobj, "trends" .= timelineobj ]
      html = do
        setTitle "Corydalis: lens information"
        $(widgetFile "lensinfo")
  defaultLayoutJson html (return obj)

getLensStatsR :: Handler TypedContent
getLensStatsR = do
  pics <- getPics
  -- TODO: resolve deSymbolize
  let bylens = Map.mapKeys deSymbolizeItem' $ getByLens pics
      lenses = Map.toList bylens
      -- TODO: resolve deSymbolize
      obj = buildCamLensStats lensOthers 30 10 lensShortName (deSymbolizeItem' . liName) bylens
  let html = do
        setTitle "Corydalis: lens statistics"
        $(widgetFile "lensstats")
  defaultLayoutJson html (return obj)
