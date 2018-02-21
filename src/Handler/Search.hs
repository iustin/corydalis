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
import qualified Data.Text       as Text


atomDescription :: Atom -> Text
atomDescription (Country  place) = "country is "  <> place
atomDescription (Province place) = "province is " <> place
atomDescription (City     place) = "city is "     <> place
atomDescription (Location place) = "location is " <> place
atomDescription (Person who) = formatPerson False who <>
                               " is in the picture"
atomDescription (Keyword what) = "tagged with keyword " <> what
atomDescription (Year year) = "taken in the year " <> Text.pack (show year)

getParams :: Handler [(Text, Text)]
getParams = reqGetParams <$> getRequest

getSearchFoldersR :: Handler Html
getSearchFoldersR = do
  config <- getConfig
  params <- getParams
  ato <- foldM (\atoms (kind, param) -> do
                  p <- lookupGetParam param
                  case p of
                    Nothing -> return atoms
                    Just v -> case buildAtom kind v of
                                Nothing -> invalidArgs [v]
                                Just a  -> return $ a:atoms
               ) [] atomNames
  let flt = map folderSearchFunction ato
      search_string = Text.intercalate " and " $ map atomDescription ato
  pics <- getPics
  let folders = filter (\p -> all (\fn -> fn p) flt) . Map.elems . repoDirs $ pics
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setTitle . toHtml $ ("Corydalis: searching folders"::Text)
    $(widgetFile "searchfolders")

getSearchImagesR :: Handler Html
getSearchImagesR = do
  config <- getConfig
  params <- getParams
  ato <- foldM (\atoms (kind, param) -> do
                  p <- lookupGetParam param
                  case p of
                    Nothing -> return atoms
                    Just v -> case buildAtom kind v of
                                Nothing -> invalidArgs [v]
                                Just a  -> return $ a:atoms
               ) [] atomNames
  let flt = map imageSearchFunction ato
      search_string = Text.intercalate " and " $ map atomDescription ato
  pics <- getPics
  let images = filterImagesBy (\i -> all (\fn -> fn i) flt) pics
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setTitle . toHtml $ ("Corydalis: searching folders"::Text)
    $(widgetFile "searchimages")
