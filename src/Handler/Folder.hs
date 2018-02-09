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

module Handler.Folder
  ( getFolderR
  ) where

import Import
import Pics
import Types
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Text as T

getFolderR :: Text -> Handler Html
getFolderR name = do
  config <- getConfig
  (pics, dir) <- getPicsAndFolder name
  let allpaths = pdMainPath dir:pdSecPaths dir
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    let stats = computeFolderStats dir
        rbuilder = (const .) FolderR
    setTitle . toHtml $ "Corydalis: folder " `T.append` name
    $(widgetFile "folder")
