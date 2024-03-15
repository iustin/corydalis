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
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}


module Handler.Search ( getQuickSearchR
                      , getSearchFoldersByYearR
                      , getSearchFoldersNoYearR
                      , getSearchR
                      , getSearchViewR
                      ) where

import qualified Data.Map        as Map
import qualified Data.Text       as Text

import           Handler.Utils
import           Handler.Widgets (noItemsFound)
import           Import
import           Indexer
import           Pics            (imgName, imgParent)

-- | Default presentation when no preference is found.
defaultPresentation :: ViewPresentation
defaultPresentation = PresentationGrid

-- | Dispatch for folders.
handlerFolders :: ViewPresentation -> Route App
handlerFolders PresentationGrid = BrowseFoldersR 0
handlerFolders PresentationList = ListFoldersR

-- | Dispatch for images.
handlerImages :: ViewPresentation -> Route App
handlerImages PresentationGrid = BrowseImagesR 0
handlerImages PresentationList = ListImagesR

-- | Computes the best handler, given a potential view mode and
-- presence of files in the results.
getBestHandler :: Maybe ViewMode -> Bool -> Route App
getBestHandler Nothing True                = handlerImages defaultPresentation
getBestHandler Nothing False               = handlerFolders defaultPresentation
getBestHandler (Just (ViewFolders p)) _    = handlerFolders p
getBestHandler (Just (ViewImages p)) False = handlerFolders p
getBestHandler (Just (ViewImages p)) True  = handlerImages p

-- | Computes best folder presentation, based on existing preference.
getBestFolderHandler :: Maybe ViewMode -> Route App
getBestFolderHandler Nothing                = handlerFolders defaultPresentation
getBestFolderHandler (Just (ViewFolders p)) = handlerFolders p
getBestFolderHandler (Just (ViewImages p))  = handlerFolders p

getSearchViewR :: Handler Html
getSearchViewR = do
    (ctx, _, _, atom, search_string, pics) <- searchContext
    images <- fst <$> liftIO (searchImages ctx atom pics)
    case Map.minView images of
      Just (i, _) -> redirect (ViewR (imgParent i) (imgName i))
      Nothing     -> defaultLayout $ do
        setHtmlTitle "Searching images"
        [whamlet|
          <h1>Image search failed
          ^{noItemsFound search_string True}
         |]

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
      viewMode <- getLastViewMode
      let handler = getBestHandler viewMode (atomFindsFiles atom')
      redirect (handler, atomToParams atom')

getSearchFoldersByYearR :: Integer -> Handler Html
getSearchFoldersByYearR year = do
  viewMode <- getLastViewMode
  redirect (getBestFolderHandler viewMode, [(symbolName TYear, sformat int year)])

getSearchFoldersNoYearR :: Handler Html
getSearchFoldersNoYearR = do
  viewMode <- getLastViewMode
  redirect (getBestFolderHandler viewMode, [(negSymbolName TYear, "")])

getSearchR :: Handler Html
getSearchR = do
  (_, _, params, atom, _, _) <- searchContext
  viewMode <- getLastViewMode
  let findsImages = atomFindsFiles atom
      handler = getBestHandler viewMode findsImages
  redirect (handler, params)
