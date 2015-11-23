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

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import Data.Prefix.Units

-- | Formats a double as a percent value. NaN values are transformed
-- into a Nothing.
formatPercent :: Double -> Maybe String
formatPercent v | isNaN v = Nothing
                | otherwise = Just $ printf "%.02f" v

showBinary :: (Integral a) => a -> String
showBinary = (++ "B") . showValue FormatBinary . convert
  where convert :: (Integral a) => a -> Int
        convert = fromIntegral

fcName :: FolderClass -> Text
fcName FolderRaw         = "raw"
fcName FolderStandalone  = "standalone"
fcName FolderUnprocessed = "not fully processed"
fcName FolderProcessed   = "fully processed"
fcName FolderEmpty       = "empty"
fcName FolderMixed       = "mixed"
fcName FolderOutdated    = "outdated"

fcDescription :: FolderClass -> Text
fcDescription FolderRaw         = "contains only unprocessed RAW files"
fcDescription FolderStandalone  = "contains only files without a RAW format"
fcDescription FolderUnprocessed = "contains unprocessed RAW files (and possibly \
                                   \standalone files)"
fcDescription FolderProcessed   = "contains RAW files, all processed"
fcDescription FolderEmpty       = "contains no image files"
fcDescription FolderMixed       = "contains both RAW files (processed) \
                                  \and files without RAW storage"
fcDescription FolderOutdated    = "contains RAW files, all processed, but \
                                  \some of the processed files are outdated \
                                  \(corresponding RAW file has been retouched \
                                  \more recently)"

showTimestamp :: NominalDiffTime -> Text
showTimestamp ts =
  T.pack $ ft ++ pico
    where ts' = posixSecondsToUTCTime ts
          pico = take 4 $ formatTime defaultTimeLocale "%q" ts'
          ft = formatTime defaultTimeLocale "%F %T." ts'

showFileTimestamp :: Maybe File -> Text
showFileTimestamp = maybe "" (showTimestamp . fileMTime)

showFileLatestTS :: Maybe File -> Text
showFileLatestTS = maybe "" (showTimestamp . fileLastTouch)

imgRowClass :: Image -> Text
imgRowClass img =
  case imgStatus img of
   ImageOutdated -> "warning"
   ImageOrphaned -> "danger"
   _ -> ""

getConfig :: Handler Config
getConfig = appConfig . appSettings <$> getYesod

getPics :: Handler Repository
getPics = do
  config <- getConfig
  liftIO $ scanAll config

getFolder :: Text -> Handler PicDir
getFolder folder = do
  pics <- getPics
  case Map.lookup folder pics of
   Nothing -> notFound
   Just dir -> return dir

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

getReloadR :: Handler Html
getReloadR = do
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
  case Map.lookup iname (pdImages dir) of
    Nothing -> notFound
    Just img -> defaultLayout $ do
      setTitle . toHtml $ "Corydalis: Image" `T.append` folder
                 `T.append` "/" `T.append` imgName img
      $(widgetFile "image")

getUntrackedR :: Text -> Text -> Handler Html
getUntrackedR folder uname = do
  dir <- getFolder folder
  case Map.lookup uname (pdUntracked dir) of
    Nothing -> notFound
    Just untrk -> defaultLayout $ do
      setTitle . toHtml $ "Corydalis: Untracked file " `T.append` folder
                 `T.append` "/" `T.append` uname
      $(widgetFile "untracked")

showFile :: Maybe Text -> Pics.File -> Widget
showFile prefix f = do
  let expanded_prefix = maybe "" (`T.append` " ") prefix
  $(widgetFile "showfile")
