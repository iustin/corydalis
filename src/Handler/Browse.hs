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
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Handler.Browse ( getBrowseFoldersR
                      , getBrowseImagesR
                      ) where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map        as Map

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Indexer
import           Pics

type ViewHandler = Int -> Route App

data GridItem a = GridItem
  { handler      :: ViewHandler
  , list_handler :: ViewHandler
  , alt_handler  :: ViewHandler
  , alt_listh    :: ViewHandler
  , retrieve     :: Ctx -> Atom -> Repository -> IO [a]
  , render       :: Config -> Int -> UrlParams -> Atom -> [a] -> Widget
  , elem_text    :: Text
  , alt_text     :: Text
  , is_images    :: Bool
  }

picDirGridItem :: GridItem PicDir
picDirGridItem = GridItem
  { handler = BrowseFoldersR
  , list_handler = const ListFoldersR
  , alt_handler = BrowseImagesR
  , alt_listh = const ListImagesR
  , retrieve = \_ atom pics -> return . Map.elems $ buildFolderMap atom pics
  , render = \_ size params atom elems -> folderGrid size params atom elems
  , elem_text = "folders"
  , alt_text = "image"
  , is_images = False
  }

imageGridItem :: GridItem Image
imageGridItem = GridItem
  { handler = BrowseImagesR
  , list_handler = const ListImagesR
  , alt_handler = BrowseFoldersR
  , alt_listh = const ListFoldersR
  , retrieve = \ctx atom pics -> Map.elems . fst <$> searchImages ctx atom pics
  , render = \config size params _ elems -> imageGrid config size params elems
  , elem_text = "images"
  , alt_text = "folder"
  , is_images = True
  }

browseHandler :: GridItem a -> Int -> Handler Html
browseHandler GridItem{..} page = do
  when (page < 0) $
    invalidArgs ["Negative page index"]
  (ctx, config, params, atom, search_string, pics) <- searchContext
  elems' <- liftIO $ retrieve ctx atom pics
  let pageSize = cfgPageSize config
      currentIdx = page * pageSize
      currentStart = currentIdx + 1
      (elems, remElems) = splitAt pageSize . drop currentIdx $ elems'
      imagesize = cfgBrowsingSize config
      nextPage = page + 1
      check_imflt = is_images
      show_alt_view = check_imflt || atomFindsFiles atom
      can_find_elems = not check_imflt || atomFindsFiles atom
  debug <- encodeToLazyText . appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    setHtmlTitle $ "browsing " <> elem_text
    $(widgetFile "browse")

getBrowseFoldersR :: Int -> Handler Html
getBrowseFoldersR = browseHandler picDirGridItem

getBrowseImagesR :: Int -> Handler Html
getBrowseImagesR = browseHandler imageGridItem
