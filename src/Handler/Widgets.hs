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
{-# LANGUAGE NoCPP             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

module Handler.Widgets where

import qualified Data.Map      as Map
import qualified Data.Text     as Text

import           Exif
import           Handler.Utils
import           Import
import           Indexer
import           Pics
import           Types

showFile :: Pics.File -> Widget
showFile f =
  $(widgetFile "showfile")

searchDiv :: Symbol -> Text -> Text -> (Text -> Text) -> [(Text, Integer)] -> Widget
searchDiv atom kindPlCap kind formatter items =
  $(widgetFile "searchdiv")

folderCover :: Int -> UrlParams -> Atom -> PicDir -> Widget
folderCover thumbsize params atom folder = do
  let name = pdName folder
      images = imageSearchFunction atom `filter` Map.elems (pdImages folder)
  case images of
    []    -> toWidget [hamlet|<span .disabled>N/A|]
    img:_ -> imageBytes thumbsize params name (imgName img)

imageBytes :: Int -> UrlParams -> Text -> Text -> Widget
imageBytes thumbsize params folder image =
  toWidget [hamlet|<a href=@?{(ViewR folder image, params)}>
                     <img
                       src="@?{(ImageBytesR folder image, [("res", Text.pack $ show thumbsize)])}"
                       style="width: #{thumbsize}px; height: #{thumbsize}px"
                       >|]

generatePrevNext :: (Ord k) => k -> Map k v -> (k -> v -> Route App) -> Widget
generatePrevNext k m r = do
  let prevRoute = uncurry r <$> Map.lookupLT k m
      nextRoute = uncurry r <$> Map.lookupGT k m
  $(widgetFile "prevnext")

imageList :: Int -> UrlParams -> Bool -> Bool -> [Image] -> Widget
imageList thumbsize params showParent hideType images = do
  let exifs = map imgExif images
      cameras = countItems . map exifCamera $ exifs
      numCameras = length cameras
      lenses = countItems . map exifLens $ exifs
      numLenses = length lenses
      sortColumns = toJSON $
                    if showParent
                      then [[1, 0], [2, 0]]
                      else [[1, 0]]::[[Int]]
  $(widgetFile "imagelist")
