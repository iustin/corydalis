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

module Handler.Search ( getQuickSearchR
                      , getSearchFoldersByYearR
                      , getSearchFoldersNoYearR
                      ) where

import qualified Data.Text     as Text

import           Handler.Utils
import           Import
import           Indexer

getQuickSearchR :: Handler Html
getQuickSearchR = do
  q <- lookupGetParam "q"
  search <- case q of
    Nothing -> invalidArgs ["Missing search parameter ('q')"]
    Just "" -> invalidArgs ["Empty search parameter"]
    Just q' -> return q'
  pics <- getPics
  (skipped, atom) <- case genQuickSearchParams pics search of
    Left err -> invalidArgs [err]
    Right p' -> return p'
  case atom of
    Nothing -> defaultLayout $ do
      setHtmlTitle "quick search"
      $(widgetFile "quicksearchfail")
    Just atom' -> do
      unless (null skipped) $ do
        setMessage . toHtml $
          "The following filters had no results so they were skipped: " <>
          ", " `Text.intercalate` map atomDescription skipped <> "."
        setSession msgTypeKey msgWarning
      let handler = if atomFindsFiles atom'
                    then BrowseImagesR
                    else BrowseFoldersR
      redirect (handler 0, atomToParams atom')

getSearchFoldersByYearR :: Integer -> Handler Html
getSearchFoldersByYearR year =
  redirect (BrowseFoldersR 0, [(symbolName TYear, Text.pack $ show year)])

getSearchFoldersNoYearR :: Handler Html
getSearchFoldersNoYearR =
  redirect (BrowseFoldersR 0, [(negSymbolName TYear, "")])
