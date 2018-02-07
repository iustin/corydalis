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
{-# LANGUAGE QuasiQuotes #-}

module Handler.Utils where

import Import
import Pics
import Types
import Exif

import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Time
import Data.Time.Clock.POSIX
import Text.Printf
import Data.Prefix.Units
import Text.Blaze (ToMarkup, Markup, toMarkup, string)

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

fcDescription :: FolderClass -> Text
fcDescription FolderRaw         = "contains only unprocessed RAW files"
fcDescription FolderStandalone  = "contains only files without a RAW format"
fcDescription FolderUnprocessed = "contains unprocessed RAW files (and possibly \
                                   \standalone files)"
fcDescription FolderProcessed   = "contains RAW files, all processed"
fcDescription FolderEmpty       = "contains no image files"
fcDescription FolderMixed       = "contains both RAW files (processed) \
                                  \and files without RAW storage"

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
   ImageOrphaned -> "danger"
   _ -> ""

getConfig :: Handler Config
getConfig = appConfig . appSettings <$> getYesod

getPics :: Handler Repository
getPics = do
  cheapRepo <- liftIO getRepo
  case cheapRepo of
    Just repo -> return repo
    Nothing -> do
      config <- getConfig
      liftIO $ scanAll config

reloadPics :: Handler ()
reloadPics = do
  config <- getConfig
  _ <- liftIO $ forceScanAll config
  return ()

getPicsAndFolder :: Text -> Handler (Repository, PicDir)
getPicsAndFolder folder = do
  pics <- getPics
  case Map.lookup folder (repoDirs pics) of
   Nothing -> notFound
   Just dir -> return (pics, dir)

getFolder :: Text -> Handler PicDir
getFolder = fmap snd . getPicsAndFolder

getFolderImage :: PicDir -> Text -> Handler Image
getFolderImage dir iname =
  case Map.lookup iname (pdImages dir) of
    Nothing -> notFound
    Just img -> return img

getImage :: Text -> Text -> Handler Image
getImage folder iname = do
  dir <- getFolder folder
  getFolderImage dir iname

showFile :: Pics.File -> Widget
showFile f =
  $(widgetFile "showfile")

folderCover :: Int -> PicDir -> Widget
folderCover thumbsize folder = do
  let name = pdName folder
  case Map.lookupMin $ pdImages folder of
    Nothing -> toWidget [hamlet|<span .disabled>N/A|]
    Just (_, img) -> imageBytes thumbsize name (imgName img)

imageBytes :: Int -> Text -> Text -> Widget
imageBytes thumbsize folder image =
  toWidget [hamlet|<a href=@{ViewR folder image}>
                     <img
                       src="@?{(ImageBytesR folder image, [("res", T.pack $ show thumbsize)])}"
                       style="width: #{thumbsize}px; height: #{thumbsize}px"
                       >|]

generatePrevNext :: (Ord k) => k -> Map k v -> (k -> v -> Route App) -> Widget
generatePrevNext k m r = do
  let prevRoute = uncurry r <$> Map.lookupLT k m
      nextRoute = uncurry r <$> Map.lookupGT k m
  $(widgetFile "prevnext")

-- | Quotes content such that copy-paste is easier if within a span.
quoteMarkup :: (ToMarkup a) => a -> Markup
quoteMarkup element = toMarkup [quote, toMarkup element, quote]
  where quote = string "'"

-- | Formats a \"Doe\/John\" name as \"John Doe\" or \"John D.\".
formatPerson :: Bool -> Text -> Text
formatPerson abbrev name =
  let w = "/" `T.splitOn` name
  in case w of
       [l, f] | not (T.null l) ->
                if abbrev
                  then f `T.append` " " `T.snoc` T.head l `T.snoc` '.'
                  else f `T.append` " " `T.append` l
       _ -> name

folderLocations :: PicDir -> Text
folderLocations =
  T.intercalate ", " . Map.keys . gExifLocations . pdExif

folderPeople :: PicDir -> Text
folderPeople =
  T.intercalate ", " . map (formatPerson True) . Map.keys . gExifPeople . pdExif

folderKeywords :: PicDir -> Text
folderKeywords =
  T.intercalate ", " . Map.keys . gExifKeywords . pdExif
