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

module Handler.Folder
  ( getFolderR
  ) where

import           Exif
import           Handler.Utils
import           Handler.Widgets
import           Import
import           Pics
import           Types

import qualified Data.Map        as Map

getFolderR :: Text -> Handler Html
getFolderR name = do
  config <- getConfig
  (pics, dir) <- getPicsAndFolder name
  params <- getParams
  let allpaths = pdMainPath dir:pdSecPaths dir
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    let stats = computeFolderStats dir
        rbuilder = (const .) FolderR
        images = map snd . Map.toList $ pdImages dir
        exifs = map imgExif images
        cameras = countItems . map exifCamera $ exifs
        lenses = countItems . map exifLens $ exifs
    setHtmlTitle $ "folder " <> name
    $(widgetFile "folder")
