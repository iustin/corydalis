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

module Handler.Browse ( getBrowseImagesR
                      ) where

import           Data.Aeson.Text (encodeToLazyText)
import qualified Data.Map.Strict as Map

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Indexer

getBrowseImagesR :: Int -> Handler Html
getBrowseImagesR page = do
  when (page < 0) $
    invalidArgs ["Negative page index"]
  (ctx, config, params, atom, search_string, pics) <- searchContext
  elems' <- liftIO $ Map.elems . fst <$> searchImages ctx atom pics
  let pageSize = cfgPageSize config
      -- Computed as an Integer, as it can be past the largest Int
      -- (for a crafted page number). A page past the last image is
      -- empty, as any other page past the end: past the largest Int
      -- (which fromInteger would wrap around), directly; otherwise, as
      -- drop then leaves nothing.
      currentIdx = toInteger page * toInteger pageSize
      currentStart = currentIdx + 1
      (elems, remElems)
        | currentIdx > toInteger (maxBound :: Int) = ([], [])
        | otherwise = splitAt pageSize . drop (fromInteger currentIdx) $ elems'
      imagesize = cfgBrowsingSize config
      nextPage = page + 1
      can_find_elems = atomFindsFiles atom
  debug <- encodeToLazyText . appShouldLogAll . appSettings <$> getYesod
  defaultLayout $ do
    setHtmlTitle "browsing images"
    $(widgetFile "browse")
