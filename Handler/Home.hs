{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Pics

import Data.List
import Text.Printf

getHomeR :: Handler RepHtml
getHomeR = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let unprocessed = computeUnprocessed pics
      allnefs = foldl' (\s n -> s + nfNefCount n) 0 unprocessed
      standalone = computeStandaloneJpegs pics
      jstandalone = foldl' (\s n -> s + jpJpegCount n) 0 standalone
  defaultLayout $ do
    setTitle "<PicMan>"
    $(widgetFile "homepage")

getUnprocessedR :: Handler RepHtml
getUnprocessedR = do
  config <- extraConfig `fmap` getExtra
  pics <- liftIO $ scanAll config
  let unprocessed = computeUnprocessed pics
      allnefs = foldl' (\s n -> s + nfNefCount n) 0 unprocessed
      allnefs' = fromIntegral allnefs::Double
      npairs = map (\n -> (n, printf "%.02f" $
                              fromIntegral (nfNefCount n) * 100 / allnefs'
                                ::String))
               unprocessed
  let handlerName = "getHomeR" :: Text
  defaultLayout $ do
    setTitle "<PicMan>"
    $(widgetFile "unprocessed")
