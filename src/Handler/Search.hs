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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Search ( getSearchFoldersR
                      , getSearchImagesR
                      ) where

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Indexer
import           Pics
import           Types

import qualified Data.Map        as Map

searchContext :: Handler (Config, [(Text, Text)], Atom, Text, Repository)
searchContext = do
  config <- getConfig
  (params, atom) <- getAtomParams
  let search_string = atomDescription atom
  pics <- getPics
  return (config, params, atom, search_string, pics)

getSearchFoldersR :: Handler Html
getSearchFoldersR = do
  (config, params, atom, search_string, pics) <- searchContext
  let folders = filter (folderSearchFunction atom) . Map.elems . repoDirs $ pics
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setTitle . toHtml $ ("Corydalis: searching folders"::Text)
    $(widgetFile "searchfolders")

getSearchImagesR :: Handler Html
getSearchImagesR = do
  (config, params, atom, search_string, pics) <- searchContext
  images <- Map.elems <$> lift (searchImages params atom pics)
  let thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setTitle . toHtml $ ("Corydalis: searching images"::Text)
    $(widgetFile "searchimages")
