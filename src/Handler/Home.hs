{-# LANGUAGE TupleSections, OverloadedStrings, NoCPP #-}

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

module Handler.Home where

import Import
import Pics
import Types
import Handler.Utils

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time

getHomeR :: Handler Html
getHomeR = do
  pics <- getPics
  let (Stats unprocessed standalone processed outdated orphaned untracked
             rawsize procsize standalonesize sidecarsize _, fcm) =
          computeRepoStats pics
      allpics = unprocessed + standalone + processed + outdated
      fstats = Map.toAscList fcm
      numfolders = Map.size pics
      all_fc = [minBound..maxBound]
  defaultLayout $ do
    setTitle "Corydalis: home"
    $(widgetFile "homepage")

getFolderR :: Text -> Handler Html
getFolderR name = do
  dir <- getFolder name
  let allpaths = pdMainPath dir:pdSecPaths dir
  defaultLayout $ do
    let stats = computeFolderStats dir
    setTitle . toHtml $ "Corydalis: folder " `T.append` name
    $(widgetFile "folder")

getBrowseFoldersR :: [FolderClass] -> Handler Html
getBrowseFoldersR kinds = do
  pics <- getPics
  let kinds_string = T.intercalate ", " . map fcName $ kinds
      folders = filterDirsByClass kinds pics
      stats = foldl' sumStats zeroStats . map computeFolderStats $ folders
      allpics = sum . map numPics $ folders
      -- allraws = sum . map numRawPics $ folders
      allunproc = sum . map numUnprocessedPics $ folders
      allprocessed = sum . map numProcessedPics $ folders
      allstandalone = sum . map numStandalonePics $ folders
      alloutdated = sum . map numOutdatedPics $ folders
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
  defaultLayout $ do
    setTitle . toHtml $
      "Corydalis: browsing folders of type " `T.append` kinds_string
    $(widgetFile "browsefolders")

postReloadR :: Handler Html
postReloadR = do
  config <- getConfig
  _ <- liftIO $ forceScanAll config
  setMessage "Cache reloaded"
  redirect HomeR

getTimelineR :: Handler Html
getTimelineR = do
  pics <- getPics
  let timeline = computeTimeLine pics
      days = Map.toAscList timeline
      tstats = do -- Maybe monad in order to avoid unsafe min/max functions
        firstday <- (fst . fst) <$> Map.minViewWithKey timeline
        lastday <- (fst . fst) <$> Map.maxViewWithKey timeline
        let numdays = diffDays lastday firstday
        return (firstday, lastday, numdays)
      formatDay = formatTime defaultTimeLocale "%F"
  defaultLayout $ do
    setTitle "Corydalis: timeline stats"
    $(widgetFile "timeline")

getSettingsR :: Handler Html
getSettingsR = do
  config <- getConfig
  let quoteString path = "'" ++ path ++ "'"
  defaultLayout $ do
    setTitle "Corydalis: Settings"
    $(widgetFile "settings")

getImageR :: Text -> Text -> Handler Html
getImageR folder iname = do
  dir <- getFolder folder
  let images = pdImages dir
  img <- case Map.lookup iname images of
           Nothing -> notFound
           Just img' -> return img'
  let imgPrev = Map.lookupLT iname images
      imgNext = Map.lookupGT iname images
  defaultLayout $ do
    setTitle . toHtml $ "Corydalis: Image " `T.append` folder
               `T.append` "/" `T.append` imgName img
    $(widgetFile "image")

getUntrackedR :: Text -> Text -> Handler Html
getUntrackedR folder uname = do
  dir <- getFolder folder
  untrk <- case Map.lookup uname (pdUntracked dir) of
             Nothing -> notFound
             Just untrk' -> return untrk'
  defaultLayout $ do
      setTitle . toHtml $ "Corydalis: Untracked file " `T.append` folder
                 `T.append` "/" `T.append` uname
      $(widgetFile "untracked")
