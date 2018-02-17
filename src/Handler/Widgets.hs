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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoCPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Widgets where

import Import
import Pics
import Indexer
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Text as T

showFile :: Pics.File -> Widget
showFile f =
  $(widgetFile "showfile")

searchDiv :: AtomType -> Text -> Text -> (Text -> Text) -> [(Text, Integer)] -> Widget
searchDiv atom kindPlCap kind formatter items =
  $(widgetFile "searchdiv")

folderCover :: Int -> PicDir -> Widget
folderCover thumbsize folder = do
  let name = pdName folder
  case Map.lookupMin $ pdImages folder of
    Nothing -> toWidget [hamlet|<span .disabled>N/A|]
    Just (_, img) -> imageBytes thumbsize name (imgName img)

imageBytes :: Int -> Text -> Text -> Widget
imageBytes thumbsize folder image =
  toWidget [hamlet|<a href=@{ViewR folder image}>
                     <img
                       src="@?{(ImageBytesR folder image, [("res", T.pack $ show thumbsize)])}"
                       style="width: #{thumbsize}px; height: #{thumbsize}px"
                       >|]

generatePrevNext :: (Ord k) => k -> Map k v -> (k -> v -> Route App) -> Widget
generatePrevNext k m r = do
  let prevRoute = uncurry r <$> Map.lookupLT k m
      nextRoute = uncurry r <$> Map.lookupGT k m
  $(widgetFile "prevnext")
