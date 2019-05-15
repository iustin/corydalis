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
                      , getQuickSearchR
                      , getSearchFoldersByYearR
                      , getSearchFoldersNoYearR
                      ) where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map        as Map
import qualified Data.Text       as Text

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Indexer
import           Pics
import           Types

searchContext :: Handler (Config, [(Text, Text)], Atom, Text, Repository)
searchContext = do
  config <- getConfig
  (_, atom) <- getAtomParams
  let search_string = atomDescription atom
  pics <- getPics
  return (config, atomToParams atom, atom, search_string, pics)

getSearchFoldersR :: Handler Html
getSearchFoldersR = do
  (config, params, atom, search_string, pics) <- searchContext
  let folders = filter (folderSearchFunction atom) . Map.elems . repoDirs $ pics
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setHtmlTitle "searching folders"
    $(widgetFile "searchfolders")

getSearchImagesR :: Int -> Handler Html
getSearchImagesR page = do
  when (page < 0) $
    invalidArgs ["Negative page index"]
  (config, params, atom, search_string, pics) <- searchContext
  images' <- Map.elems <$> liftIO (searchImages atom pics)
  let pageSize = cfgPageSize config
      currentIdx = page * pageSize
      currentStart = currentIdx + 1
      (images, remImages) = splitAt pageSize . drop currentIdx $ images'
      imagesize = cfgBrowsingSize config
      nextPage = page + 1
  -- TODO: allow switching between grid and list, so that people can
  -- choose either [feature].
  debug <- encodeToLazyText . appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    addScript $ StaticR masonry_js_masonry_pkgd_js
    addScript $ StaticR imagesloaded_js_imagesloaded_pkgd_js
    addScript $ StaticR infinite_scroll_js_infinite_scroll_pkgd_js
    addScript $ StaticR corydalis_js_imagegrid_js
    setHtmlTitle "searching images"
    $(widgetFile "searchimages")

getQuickSearchR :: Handler Html
getQuickSearchR = do
  q <- lookupGetParam "q"
  search <- case q of
    Nothing -> invalidArgs ["Missing search parameter ('q')"]
    Just "" -> invalidArgs ["Empty search parameter"]
    Just q' -> return q'
  pics <- getPics
  (skipped, params) <- case genQuickSearchParams pics search of
    Left err -> invalidArgs [err]
    Right p' -> return p'
  case params of
    Nothing -> defaultLayout $ do
      setHtmlTitle "quick search"
      $(widgetFile "quicksearchfail")
    Just p -> do
      unless (null skipped) $ do
        setMessage . toHtml $
          "The following filters had no results so they were skipped: " <>
          ", " `Text.intercalate` map atomDescription skipped <> "."
        setSession msgTypeKey msgWarning
      redirect (SearchImagesR 0, p)

getSearchFoldersByYearR :: Integer -> Handler Html
getSearchFoldersByYearR year =
  redirect (SearchFoldersR, [(symbolName TYear, Text.pack $ show year)])

getSearchFoldersNoYearR :: Handler Html
getSearchFoldersNoYearR =
  redirect (SearchFoldersR, [(negSymbolName TYear, "")])
