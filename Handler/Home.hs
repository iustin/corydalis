{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Pics

import Data.List
import qualified Data.Map as Map
import Text.Printf

formatPercent :: Double -> String
formatPercent = printf "%.02f"

getHomeR :: Handler Html
getHomeR = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let allpics = totalPics pics
      allnefs = totalRawPics pics
      standalone = totalStandalonePics pics
      unprocessed = totalUnprocessedPics pics
      processed = totalProcessedPics pics
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

getFolderR :: String -> Handler Html
getFolderR name = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  case Map.lookup name pics of
    Nothing -> notFound
    Just dir -> defaultLayout $ do
      setTitle . toHtml $ "PicMan: folder " ++ name
      $(widgetFile "folder")
