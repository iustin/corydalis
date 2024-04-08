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
import qualified Data.Set      as Set
import qualified Data.Text     as Text

import           Exif          (gExifFlashMode, gExifFlashSrc, lensShortName,
                                liName, liSpec, unknownLens)
import           Handler.Items
import           Handler.Utils
import           Import        hiding (Status)
import           Indexer
import           Pics

data XGraphData a b = XGraphData
  { xgdName       :: Text
  , xgdType       :: Text
  , xgdX          :: [a]
  , xgdY          :: [b]
  , xgdText       :: Maybe [Text]
--  , gdYAxis :: Maybe Text
  , xgdMode       :: Maybe Text
  , xgdYaxis      :: Maybe Text
  , xgdMarkerSize :: Maybe Int
  }

instance Default (XGraphData a b) where
  def = XGraphData { xgdName = ""
                   , xgdType = "scatter"
                   , xgdX = []
                   , xgdY = []
                   , xgdText = Nothing
                   , xgdMode = Nothing
                   , xgdYaxis = Nothing
                   , xgdMarkerSize = Just 15
                  }

instance (ToJSON a, ToJSON b) => ToJSON (XGraphData a b) where
  toJSON XGraphData {..} =
    object [ "name"  .= xgdName
           , "type"  .= xgdType
           , "x"     .= xgdX
           , "y"     .= xgdY
           , "text"  .= xgdText
           , "yaxis" .= xgdYaxis
           , "mode"  .= xgdMode
           , "marker" .= maybe (object []) (\ms -> object [ "size" .= map (const ms) xgdX]) xgdMarkerSize
           ]

-- | Holds words that should be filtered out from the lens graph.
--
-- All vendors are going overboard… this is not helpful for the
-- graph. Full text remains available in lens list.
hideLensWords :: Set Text
hideLensWords = Set.fromList
  [ "AF-S"
  , "VR"
  , "M.Zuiko"
  , "Digital"
  , "AF"
  , "ED"
  , "DG"
  , "Asph."
  , "Power"
  , "OIS"
  , "Pro"
  , "IS"
  , "FL"
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
      buildTop10 m n = let allItems = sortBy (flip compare) $
                             Map.foldlWithKey' (\a k (Occurrence cnt sz _ _ _ _) ->
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
      top10l = buildTopNItems (unknownLens { liName = "others", liSpec = "others" })
                 bylens 12
      top10l' = map (\(a,b,txt,d, t) ->
                       let w = Text.words txt
                           w' = filter (not . (`Set.member` hideLensWords)) w
                       in (a, b, Text.unwords w', d, t)) top10l
      jsonl = foldl' (\a (cnt, _, k, li, _) ->
                        def { xgdName = lensShortName li
                            , xgdType = "bar"
                            , xgdMode = Just "markers"
                            , xgdX = [k]
                            , xgdY = [fromIntegral cnt]
                            }:a)
              ([]::[XGraphData Text Int64]) top10l'
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
      perYearStats = let stats = Map.foldl'
                          (\l f -> Map.foldl' (\l' p ->
                            let picYear = imageYear p
                                is = updateStatsWithPic zeroStats p
                            in Map.insertWith sumStats picYear is l') l (pdImages f)
                          ) Map.empty (repoDirs pics)
                      in Map.foldlWithKey' (\l k s -> (fromIntegral $ totalStatsSize s,
                                                       fromIntegral $ totalStatsCount s,
                                                       k):l) [] stats
      (yssize, yscount, ysyears) = unzip3 perYearStats
      -- FIXME: map the "no-year" case to something better. For now, use -
      -- 2, so there's a clear gap.
      no_year = maybe 0 (\x -> minimum x - 2) . fromNullable . catMaybes $ ysyears
      years_values = map (fromIntegral . fromMaybe no_year) ysyears
      json_years_count = def
        { xgdName = "Count"
        , xgdType = "scatter"
        , xgdMode = Nothing
        , xgdX = years_values
        , xgdY = map fromInteger yscount
        , xgdText = Nothing
        , xgdMarkerSize = Nothing
        }::XGraphData Int64 Int64
      json_years_size = def
        { xgdName = "Size"
        , xgdType = "scatter"
        , xgdMode = Nothing
        , xgdX = years_values
        , xgdY = map fromInteger yssize
        , xgdText = Nothing
        , xgdYaxis = Just "y2"
        , xgdMarkerSize = Nothing
        }::XGraphData Int64 Int64

      problems = topN 3 $ repoProblems pics
      exifstats = repoExif pics
      imageFilter s = (ListImagesR, atomToParams (Status s))
      folderFilter cs = (ListFoldersR, atomToParams (Any (map FClass cs)))
  let html = do
        setTitle "Corydalis: curate"
        $(widgetFile "curate")
  defaultLayoutJson html (return $ object [ "global" .= json
                                          , "folders" .= j2
                                          , "lenses"  .= jsonl
                                          , "years"   .= [json_years_size, json_years_count]
                                          ])
