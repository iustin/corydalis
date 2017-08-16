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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoCPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Utils where

import Import
import Pics
import Types

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import Data.Prefix.Units
import Text.Blaze (ToMarkup, Markup, toMarkup, string)
import Data.Aeson

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

getExifCache :: Handler RawExifCache
getExifCache = do
  exifs <- runDB $
    selectList [] []
  return $
    foldl' (\rc e ->
               let v = entityVal e
                   k = exifPath v
                   o = (decodeStrict $ exifJson v)::Maybe Object
               in case o of
                    Just obj -> Map.insert k obj rc
                    _ -> rc) Map.empty exifs

getPics :: Handler Repository
getPics = do
  config <- getConfig
  cache <- getExifCache
  liftIO $ scanAll config cache

getPicsAndFolder :: Text -> Handler (Repository, PicDir)
getPicsAndFolder folder = do
  pics <- getPics
  case Map.lookup folder (repoDirs pics) of
   Nothing -> notFound
   Just dir -> return (pics, dir)

getFolder :: Text -> Handler PicDir
getFolder = fmap snd . getPicsAndFolder

showFile :: Pics.File -> Widget
showFile f = do
  $(widgetFile "showfile")

generatePrevNext :: (Ord k) => k -> Map k v -> (k -> v -> Route App) -> Widget
generatePrevNext k m r = do
  let prevRoute = uncurry r <$> Map.lookupLT k m
      nextRoute = uncurry r <$> Map.lookupGT k m
  $(widgetFile "prevnext")

-- | Quotes content such that copy-paste is easier if within a span.
quoteMarkup :: (ToMarkup a) => a -> Markup
quoteMarkup element = toMarkup [quote, toMarkup element, quote]
  where quote = string "'"
