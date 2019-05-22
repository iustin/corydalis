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

getBrowseFoldersR :: Handler Html
getBrowseFoldersR = do
  (_, config, params, atom, search_string, pics) <- searchContext
  let folders = filter (folderSearchFunction atom) . Map.elems . repoDirs $ pics
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setHtmlTitle "searching folders"
    $(widgetFile "searchfolders")

getBrowseImagesR :: Int -> Handler Html
getBrowseImagesR page = do
  when (page < 0) $
    invalidArgs ["Negative page index"]
  (ctx, config, params, atom, search_string, pics) <- searchContext
  images' <- Map.elems <$> liftIO (searchImages ctx atom pics)
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
    addStylesheet $ StaticR fancybox_css_jquery_fancybox_css
    addScript $ StaticR masonry_js_masonry_pkgd_js
    addScript $ StaticR imagesloaded_js_imagesloaded_pkgd_js
    addScript $ StaticR infinite_scroll_js_infinite_scroll_pkgd_js
    addScript $ StaticR fancybox_js_jquery_fancybox_js
    addScript $ StaticR corydalis_js_imagegrid_js
    addScript $ StaticR corydalis_js_fancybox_js
    setHtmlTitle "searching images"
    $(widgetFile "searchimages")
