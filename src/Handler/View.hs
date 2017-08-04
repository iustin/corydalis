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
{-# LANGUAGE RecordWildCards #-}

module Handler.View ( getViewR
                    , getImageBytesR
                    , getImageInfoR
                    ) where

import Import
import Pics
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Text as T

data ImageInfo = ImageInfo
  { iiInfoUrl  :: Text
  , iiBytesUrl :: Text
  , iiViewUrl  :: Text
  , iiName     :: Text
  }

instance ToJSON ImageInfo where
  toJSON ImageInfo {..} =
    object [ "info"  .= iiInfoUrl
           , "bytes" .= iiBytesUrl
           , "view"  .= iiViewUrl
           , "name"  .= iiName
           ]

mkImage :: Text -> Text -> (Route App -> Text) -> ImageInfo
mkImage folder iname render =
  ImageInfo (render $ ImageInfoR folder iname)
            (render $ ImageBytesR folder iname)
            (render $ ViewR folder iname)
            iname

data ViewInfo = ViewInfo
  { viFolder  :: Text
  , viImage   :: Text
  , viFirst   :: ImageInfo
  , viPrev    :: ImageInfo
  , viCurrent :: ImageInfo
  , viNext    :: ImageInfo
  , viLast    :: ImageInfo
  }

instance ToJSON ViewInfo where
  toJSON ViewInfo {..} =
    object [ "folder"      .= viFolder
           , "image"       .= viImage
           , "first"       .= viFirst
           , "prev"        .= viPrev
           , "current"     .= viCurrent
           , "next"        .= viNext
           , "last"        .= viLast
           ]

getViewR :: Text -> Text -> Handler Html
getViewR folder iname = do
  dir <- getFolder folder
  let images = pdImages dir
  img <- case Map.lookup iname images of
           Nothing -> notFound
           Just img' -> return img'
  debug <- appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    addScript $ StaticR js_viewer_js
    addScript $ StaticR js_hammer_js
    setTitle . toHtml $ "Corydalis: Image " `T.append` folder
               `T.append` "/" `T.append` imgName img
    $(widgetFile "view")

getImageBytesR :: Text -> Text -> Handler ()
getImageBytesR folder iname = do
  dir <- getFolder folder
  let images = pdImages dir
  img <- case Map.lookup iname images of
           Nothing -> notFound
           Just img' -> return img'
  jpath <- case imgJpegPath img of
             j:_ -> return $ filePath j
             _   -> case (imgRawPath img, flagsSoftMaster (imgFlags img)) of
                      (Just r, True) -> return $ filePath r
                      _ -> notFound
  sendFile "image/jpeg" (T.unpack jpath)

getImageInfoR :: Text -> Text -> Handler Value
getImageInfoR folder iname = do
  dir <- getFolder folder
  let images = pdImages dir
  img <- case Map.lookup iname images of
           Nothing -> notFound
           Just img' -> return img'
  render <- getUrlRender
  let conv = maybe "" (\(k, _) -> k)
      -- since we have an image, it follows that min/max must exist
      -- (they might be the same), hence we can use the non-total
      -- functions findMin/findMax until newer containers package
      -- reaches LTS
      imgFirst = fst  $ Map.findMin images
      imgPrev  = conv $ Map.lookupLT iname images
      imgNext  = conv $ Map.lookupGT iname images
      imgLast  = fst  $ Map.findMax images
      mk = \i -> mkImage folder i render
  return . toJSON $
    ViewInfo folder (imgName img) (mk imgFirst) (mk imgPrev) (mk iname) (mk imgNext) (mk imgLast)
