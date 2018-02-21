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

module Handler.Browse
  ( getBrowseFoldersR
  , getBrowseImagesR
  ) where

import           Handler.Utils
import           Handler.Widgets
import           Import
import           Pics
import           Types

import qualified Data.Text       as Text

getBrowseFoldersR :: [FolderClass] -> Handler Html
getBrowseFoldersR kinds = do
  config <- getConfig
  pics <- getPics
  let kinds_string = Text.intercalate ", " . map fcName $ kinds
      folders = filterDirsByClass kinds pics
      stats = foldl' sumStats zeroStats . map computeFolderStats $ folders
      allpics = sum . map numPics $ folders
      -- allraws = sum . map numRawPics $ folders
      allunproc = sum . map numUnprocessedPics $ folders
      allprocessed = sum . map numProcessedPics $ folders
      allstandalone = sum . map numStandalonePics $ folders
      allorphaned = sum . map numOrphanedPics $ folders
      npairs = map (\n -> let f_class = (fcIcon . folderClass) n
                          in (n, f_class))
               folders
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setTitle . toHtml $
      "Corydalis: browsing folders of type " <> kinds_string
    $(widgetFile "browsefolders")

getBrowseImagesR :: [ImageStatus] -> Handler TypedContent
getBrowseImagesR kinds = do
  config <- getConfig
  pics <- getPics
  let kinds_string = Text.intercalate ", " . map (Text.pack . show) $ kinds
      thumbsize = cfgThumbnailSize config
      images = filterImagesByClass kinds pics
      allpaths = foldl' (\paths img ->
                           let jpaths = map filePath . imgJpegPath $ img
                               withJpegs = jpaths  ++ paths
                           in case imgRawPath img of
                             Nothing -> withJpegs
                             Just r  -> filePath r:withJpegs) [] images
  selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle . toHtml $
        "Corydalis: showing images of type " <> kinds_string
      $(widgetFile "browseimages")
    provideRep $ return $ "\n" `Text.intercalate` allpaths
