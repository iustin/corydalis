{-# LANGUAGE TupleSections, OverloadedStrings, NoCPP #-}
module Handler.Home where

import Import
import Pics

import Data.List
import qualified Data.Map as Map
import qualified Data.Text as T
import Text.Printf

formatPercent :: Double -> String
formatPercent = printf "%.02f"

fcName :: FolderClass -> Text
fcName FolderRaw = "raw"
fcName FolderStandalone = "standalone"
fcName FolderUnprocessed = "not fully processed"
fcName FolderProcessed = "fully processed"
fcName FolderEmpty = "empty"
fcName FolderMixed = "desynchronised"

fcDescription :: FolderClass -> Text
fcDescription FolderRaw = "contains only RAW files"
fcDescription FolderStandalone = "contains only files without a RAW format"
fcDescription FolderUnprocessed = "contains RAW files and some processed files"
fcDescription FolderProcessed = "contains RAW files, all processed"
fcDescription FolderEmpty = "contains no image files"
fcDescription FolderMixed = "contains both unprocessed RAW files and\
                            \ files without RAW storage (unexpected)"

getHomeR :: Handler Html
getHomeR = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let allpics = totalPics pics
      allnefs = totalRawPics pics
      standalone = totalStandalonePics pics
      unprocessed = totalUnprocessedPics pics
      processed = totalProcessedPics pics
      fstats = Map.toAscList $ computeFolderStats pics
      numfolders = length $ Map.elems pics
  defaultLayout $ do
    setTitle "<PicMan>"
    $(widgetFile "homepage")

getUnprocessedR :: Handler Html
getUnprocessedR = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let unprocessed = computeUnprocessedDirs pics
      allnefs = totalUnprocessedPics pics
      allnefs' = fromIntegral allnefs::Double
      npairs = map (\n -> let unproc = fromIntegral (numUnprocessedPics n)
                              numraw = fromIntegral (numRawPics n)
                          in (n,
                              formatPercent $ unproc * 100 / numraw,
                              formatPercent $ unproc * 100 / allnefs'))
               unprocessed
  let handlerName = "getHomeR" :: Text
  defaultLayout $ do
    setTitle "<PicMan>"
    $(widgetFile "unprocessed")

getFolderR :: Text -> Handler Html
getFolderR name = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  case Map.lookup name pics of
    Nothing -> notFound
    Just dir -> defaultLayout $ do
      setTitle . toHtml $ "PicMan: folder " `T.append` name
      $(widgetFile "folder")

getBrowseFoldersR :: [FolderClass] -> Handler Html
getBrowseFoldersR kinds = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let folders = filterDirsByClass kinds pics
      unprocessed = folders
      allnefs = totalUnprocessedPics pics
      allnefs' = fromIntegral allnefs::Double
      npairs = map (\n -> let unproc = fromIntegral (numUnprocessedPics n)
                              numraw = fromIntegral (numRawPics n)
                          in (n,
                              formatPercent $ unproc * 100 / numraw,
                              formatPercent $ unproc * 100 / allnefs'))
               folders
  defaultLayout $ do
    setTitle "<PicMan>"
    $(widgetFile "unprocessed")
