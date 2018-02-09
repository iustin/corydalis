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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoCPP #-}
{-# LANGUAGE RecordWildCards #-}

module Handler.Browse
  ( getBrowseFoldersR
  , getBrowseImagesR
  ) where

import Import
import Pics
import Types
import Handler.Utils

import qualified Data.Text as T

getBrowseFoldersR :: [FolderClass] -> Handler Html
getBrowseFoldersR kinds = do
  config <- getConfig
  pics <- getPics
  let kinds_string = T.intercalate ", " . map fcName $ kinds
      folders = filterDirsByClass kinds pics
      stats = foldl' sumStats zeroStats . map computeFolderStats $ folders
      allpics = sum . map numPics $ folders
      -- allraws = sum . map numRawPics $ folders
      allunproc = sum . map numUnprocessedPics $ folders
      allprocessed = sum . map numProcessedPics $ folders
      allstandalone = sum . map numStandalonePics $ folders
      allorphaned = sum . map numOrphanedPics $ folders
      tp = formatPercent $
           fromIntegral allunproc * 100 / fromIntegral allpics
      npairs = map (\n -> let unproc = fromIntegral (numUnprocessedPics n)
                              numraw = fromIntegral (numRawPics n)
                              f_class = show $ folderClass n
                              f_class' = fromMaybe f_class $
                                         stripPrefix "Folder" f_class
                          in ( n
                             , formatPercent $ unproc * 100 / numraw
                             , f_class'))
               folders
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    setTitle . toHtml $
      "Corydalis: browsing folders of type " `T.append` kinds_string
    $(widgetFile "browsefolders")

getBrowseImagesR :: [ImageStatus] -> Handler TypedContent
getBrowseImagesR kinds = do
  pics <- getPics
  let kinds_string = T.intercalate ", " . map (T.pack . show) $ kinds
      images = filterImagesByClass kinds pics
      allpaths = foldl' (\paths img ->
                           let jpaths = map filePath . imgJpegPath $ img
                               withJpegs = jpaths  ++ paths
                           in case imgRawPath img of
                             Nothing -> withJpegs
                             Just r -> filePath r:withJpegs) [] images
  selectRep $ do
    provideRep $ defaultLayout $ do
      setTitle . toHtml $
        "Corydalis: showing images of type " `T.append` kinds_string
      $(widgetFile "browseimages")
    provideRep $ return $ "\n" `T.intercalate` allpaths
