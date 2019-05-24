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

data GridItem a = GridItem
  { handler      :: Int -> Route App
  , alt_handler  :: Int -> Route App
  , list_handler :: Route App
  , retrieve     :: Ctx -> Atom -> Repository -> IO [a]
  , render       :: Config -> Int -> UrlParams -> Atom -> [a] -> Widget
  , elem_text    :: Text
  , alt_text     :: Text
  , is_images    :: Bool
  }

picDirGridItem :: GridItem PicDir
picDirGridItem = GridItem
  { handler = BrowseFoldersR
  , alt_handler = BrowseImagesR
  , list_handler = ListFoldersR
  , retrieve = \_ atom pics -> return . Map.elems $ buildFolderMap atom pics
  , render = \_ size params atom elems -> folderGrid size params atom elems
  , elem_text = "folders"
  , alt_text = "image"
  , is_images = False
  }

imageGridItem :: GridItem Image
imageGridItem = GridItem
  { handler = BrowseImagesR
  , alt_handler = BrowseFoldersR
  , list_handler = ListImagesR
  , retrieve = \ctx atom pics -> Map.elems <$> searchImages ctx atom pics
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
      do_fancybox = is_images
      show_alt_view = check_imflt || atomFindsFiles atom
      can_find_elems = not (check_imflt) || atomFindsFiles atom
  debug <- encodeToLazyText . appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    when do_fancybox $
      addStylesheet $ StaticR fancybox_css_jquery_fancybox_css
    addScript $ StaticR masonry_js_masonry_pkgd_js
    addScript $ StaticR imagesloaded_js_imagesloaded_pkgd_js
    addScript $ StaticR infinite_scroll_js_infinite_scroll_pkgd_js
    addScript $ StaticR corydalis_js_imagegrid_js
    when do_fancybox $ do
      addScript $ StaticR fancybox_js_jquery_fancybox_js
      addScript $ StaticR corydalis_js_fancybox_js
    setHtmlTitle $ "browsing " <> elem_text
    $(widgetFile "browse")

getBrowseFoldersR :: Int -> Handler Html
getBrowseFoldersR = browseHandler picDirGridItem

getBrowseImagesR :: Int -> Handler Html
getBrowseImagesR = browseHandler imageGridItem
