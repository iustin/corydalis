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
import           Pics

import qualified Data.Map      as Map
import qualified Data.Set      as Set

data GraphData a b c = GraphData
  { gdName  :: Text
  , gdType  :: Text
  , gdX     :: Maybe [a]
  , gdY     :: Maybe [b]
  , gdZ     :: Maybe [c]
  , gdText  :: Maybe [Text]
  , gdMode  :: Maybe Text
  , gdExtra :: [(Text, Value)]
  }

instance Default (GraphData a b c) where
  def = GraphData { gdName = ""
                  , gdType = "scatter"
                  , gdX = Nothing
                  , gdY = Nothing
                  , gdZ = Nothing
                  , gdText = Nothing
                  , gdMode = Nothing
                  , gdExtra = []
                  }

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (GraphData a b c) where
  toJSON GraphData {..} =
    object $ [ "name"  .= gdName
             , "type"  .= gdType
             , "x"     .= gdX
             , "y"     .= gdY
             , "z"     .= gdZ
             , "text"  .= gdText
             , "mode"  .= gdMode
             ] ++ map (uncurry (.=)) gdExtra

counterOne :: Int64
counterOne = 1

getLensInfoR :: Text -> Handler TypedContent
getLensInfoR lensname = do
  pics <- getPics
  let RepoStats
        (Stats _ _ _ _ _ _ _ _ _ _ _ bylens) _ =
          repoStats pics
  lens <- case lensname `Map.lookup` bylens of
            Nothing     -> notFound
            Just (l, _) -> return l
  let images = filterImagesBy (\i -> (liName . exifLens . imgExif) i == lensname) pics
      fam = foldl' (\m i ->
                       let e = imgExif i
                           fl = exifFocalLength e
                           aper = exifAperture e
                       in case (fl, aper) of
                            (Just fl', Just aper') -> Map.insertWith (+) (fl', aper') counterOne m
                            _ -> m) Map.empty images
      cameras = foldl' (\m i -> case exifCamera (imgExif i) of
                                  Nothing -> m
                                  Just c  ->  Map.insertWith (+) c counterOne m) Map.empty images
      faml = Map.toList fam
      (xys, cnt) = unzip faml
      maxCnt = fromIntegral $ maybe 5 maximum $ fromNullable cnt
      (x, y) = unzip xys
      allapertures = Set.toAscList $ Set.fromList y
      tickVals = allapertures
      tickText = map show allapertures
      hoverFmt ((fl', ap'), cnt') =
        show fl'++"mm @ f/" ++ show ap' ++ ": " ++ show cnt' ++ " images"
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
  let jsonl = def { gdName = ""
                  , gdType = "scatter"
                  , gdMode = Just "markers"
                  , gdX = Just x
                  , gdY = Just y
                  , gdExtra = [ ("colorscale", "YIGnBu")
                              , ("reversescale", Bool True)
                              , ("marker", object [ "size" .= (toJSON cnt::Value)
                                                  , "sizemin" .= toJSON (4::Int)
                                                  , "sizemode" .= String "area"
                                                  , "sizeref" .= toJSON (2.0 * maxCnt / (90 ** 2)::Double)
                                                  ])
                              , ("text", toJSON $ map hoverFmt faml)
                              , ("hoverinfo", "text")
                              ]
                  } :: GraphData Double Double Double
  let html = do
        setTitle "Corydalis: lens information"
        addPlotly
        $(widgetFile "lensinfo")
  defaultLayoutJson html (return $ object [ "lensflap"  .= [jsonl]
                                          , "ytickvals" .= tickVals
                                          , "yticktext" .= tickText
                                          ])

getLensStatsR :: Handler TypedContent
getLensStatsR = do
  pics <- getPics
  let RepoStats
        (Stats _ _ _ _ _ _ _ _ _ _ _ bylens) _ =
          repoStats pics
      lenses = Map.toList bylens
      top10l = buildTopNLenses bylens 30
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
