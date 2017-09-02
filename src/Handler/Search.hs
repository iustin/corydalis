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
{-# LANGUAGE OverloadedStrings #-}

module Handler.Search ( getSearchFoldersR
                      ) where

import Import
import Exif
import Pics
import Types
import Indexer
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time

getSearchFoldersR :: Handler Html
getSearchFoldersR = do
  let search_string = ""::Text
  flt <- foldM (\atoms (kind, param) -> do
                  p <- lookupGetParam param
                  case p of
                    Nothing -> return atoms
                    Just v -> case buildAtom kind v of
                                Nothing -> invalidArgs [v]
                                Just a -> return $ (buildSearchFunction a):atoms
               ) [] atomNames
  pics <- getPics
  let folders = filter (\p -> all (\fn -> fn p) flt) . Map.elems . repoDirs $ pics
  defaultLayout $ do
    setTitle . toHtml $ ("Corydalis: searching folders"::Text)
    $(widgetFile "searchfolders")
