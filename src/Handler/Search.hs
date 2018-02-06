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
import Pics
import Indexer
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Text as T


atomDescription :: Atom -> Text
atomDescription (Location place) = "location is " `T.append` place
atomDescription (Person who) = (formatPerson False who) `T.append`
                               " is in the picture"
atomDescription (Keyword what) = "tagged with keyword " `T.append` what
atomDescription (Year year) = "taken in the year " `T.append` (T.pack $ show year)

getSearchFoldersR :: Handler Html
getSearchFoldersR = do
  ato <- foldM (\atoms (kind, param) -> do
                  p <- lookupGetParam param
                  case p of
                    Nothing -> return atoms
                    Just v -> case buildAtom kind v of
                                Nothing -> invalidArgs [v]
                                Just a -> return $ a:atoms
               ) [] atomNames
  let flt = map buildSearchFunction ato
      search_string = T.intercalate " and " $ map atomDescription ato
  pics <- getPics
  let folders = filter (\p -> all (\fn -> fn p) flt) . Map.elems . repoDirs $ pics
  defaultLayout $ do
    setTitle . toHtml $ ("Corydalis: searching folders"::Text)
    $(widgetFile "searchfolders")
