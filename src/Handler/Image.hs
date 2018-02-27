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

module Handler.Image
  ( getImageR
  ) where

import           Exif
import           Handler.Utils
import           Handler.Widgets
import           Import
import           Pics

getImageR :: Text -> Text -> Handler Html
getImageR folder iname = do
  dir <- getFolder folder
  img <- getFolderImage dir iname
  params <- getParams
  let images = pdImages dir
  let rbuilder ik io = ImageR (imgParent io) ik
      flags = if flagsSoftMaster (imgFlags img)
                 then "soft master"::Text
                 else "(none)"
      lens = exifLens . imgExif $ img
  defaultLayout $ do
    setHtmlTitle $ "image " <> folder <> "/" <> imgName img
    $(widgetFile "image")
