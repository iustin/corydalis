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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoCPP #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.LensInfo
  ( getLensInfoR
  ) where

import Import
import Pics
import Exif
import Handler.Utils

import qualified Data.Map as Map

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
             ] ++ map (\(t, v) -> t .= v) gdExtra

getLensInfoR :: Text -> Handler TypedContent
getLensInfoR lensname = do
  pics <- getPics
  let RepoStats
        (Stats _ _ _ _ _ _ _ _ _ _ _ bylens) _ =
          repoStats pics
  lens <- case lensname `Map.lookup` bylens of
            Nothing -> notFound
            Just (l, _) -> return l
  let images = filterImagesBy (\i -> (liName . exifLens . imgExif) i == lensname) pics
      (x, y) = foldl' (\c@(ax,ay) i ->
                       let e = imgExif i
                           fl = exifFocalLength e
                           aper = exifAperture e
                       in case (fl, aper) of
                            (Just fl', Just aper') -> (fl':ax, aper':ay)
                            _ -> c) ([],[]) images
      cameras = foldl' (\m i -> let c = exifCamera (imgExif i)
                                in if c == unknown
                                   then m
                                   else  Map.insertWith (+) c (1::Int64) m) Map.empty images
  let jsonl = def { gdName = ""
                  , gdType = "histogram2dcontour"
                  , gdMode = Nothing
                  , gdX = Just x
                  , gdY = Just y
                  , gdExtra = [ ("colorscale", "YIGnBu")
                              , ("reversescale", Bool True)
                                 --toJSON $ [ ("0.0"::String, "rgb(255,255,255)"::String)
                                 --                       , ("1.0", "rgb(255,127,0)")
                                 --                       ])
                              ]
                  } :: GraphData Double Double Double
  let html = do
        setTitle "Corydalis: lens information"
        addScript $ StaticR js_plotly_js
        $(widgetFile "lensinfo")
  defaultLayoutJson html (return $ object [ "lensflap"  .= [jsonl]
                                          ])
