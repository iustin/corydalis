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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoCPP                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.List
  ( getListItemsR
  , getListFoldersR
  , getListImagesR
  ) where

import qualified Data.Map        as Map
import qualified Data.Text.Lazy  as TextL

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Indexer
import           Pics

getListItemsR :: Symbol -> Handler Html
getListItemsR atom = do
  let description = atomTypeDescriptions atom
  pics <- getPics
  let items = Map.toList $ getAtoms atom pics
  defaultLayout $ do
    setHtmlTitle $ "listing " <> description
    $(widgetFile "listitems")


getListFoldersR :: Handler Html
getListFoldersR = do
  (_, config, params, atom, search_string, pics) <- searchContext
  let folders = buildFolderMap atom pics
      fclass = map (\f -> (f, pdStats f)) folders
      -- FIXME: this version changes output compared to before; movie
      -- files and untracked were considered "processed". Review the
      -- semantics here?
      allStatPics s = sRaw s + sProcessed s + sStandalone s + sOrphaned s + sMovies s
      stats = foldl' sumStats zeroStats $ map snd fclass
      allpics = allunproc + allprocessed + allstandalone + allorphaned
      allunproc = sRaw stats
      allprocessed = sProcessed stats + sMovies stats
      allstandalone = sStandalone stats
      allorphaned = sOrphaned stats
      npairs = map (\(n, s) -> let c = folderClassFromStats s
                               in (n, s, c, fcIcon c)) fclass
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setHtmlTitle "Listing folders"
    $(widgetFile "listfolders")

getListImagesR :: Handler TypedContent
getListImagesR = do
  (ctx, config, params, atom, search_string, pics) <- searchContext
  images <- Map.elems <$> liftIO (searchImages ctx atom pics)
  let thumbsize = cfgThumbnailSize config
      allpaths = foldl' (\paths img ->
                           let jpaths = map filePath . imgJpegPath $ img
                               withJpegs = jpaths  ++ paths
                           in case imgRawPath img of
                             Nothing -> withJpegs
                             Just r  -> filePath r:withJpegs) [] images
  selectRep $ do
    provideRep $ defaultLayout $ do
      setHtmlTitle "Listing images"
      $(widgetFile "listimages")
    provideRep $ return $ "\n" `TextL.intercalate` allpaths
