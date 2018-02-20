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
{-# LANGUAGE NoCPP             #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module Handler.Utils where

import           Exif
import           Import
import           Pics
import           Types

import qualified Data.Map              as Map
import           Data.Prefix.Units
import qualified Data.Text             as Text
import           Data.Time
import           Data.Time.Clock.POSIX
import           Text.Blaze            (Markup, ToMarkup, string, toMarkup)
import           Text.Printf

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
showTimestamp =
  showLocalTime . utcToLocalTime utc . posixSecondsToUTCTime

showLocalTime :: LocalTime -> Text
showLocalTime ts =
  Text.pack $ ft ++ pico
    where pico = take 4 $ formatTime defaultTimeLocale "%q" ts
          ft = formatTime defaultTimeLocale "%F %T." ts

showExifDate :: Image -> Text
showExifDate (imgExif -> e) =
  case exifCreateDate e of
    Just lt -> showLocalTime lt
    Nothing -> "?"

showFileTimestamp :: Maybe File -> Text
showFileTimestamp = maybe "" (showTimestamp . fileMTime)

showFileLatestTS :: Maybe File -> Text
showFileLatestTS = maybe "" (showTimestamp . fileLastTouch)

imgRowClass :: Image -> Text
imgRowClass img =
  case imgStatus img of
   ImageOrphaned -> "danger"
   _             -> ""

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
   Nothing  -> notFound
   Just dir -> return (pics, dir)

getFolder :: Text -> Handler PicDir
getFolder = fmap snd . getPicsAndFolder

getFolderImage :: PicDir -> Text -> Handler Image
getFolderImage dir iname =
  case Map.lookup iname (pdImages dir) of
    Nothing  -> notFound
    Just img -> return img

getImage :: Text -> Text -> Handler Image
getImage folder iname = do
  dir <- getFolder folder
  getFolderImage dir iname

-- | Quotes content such that copy-paste is easier if within a span.
quoteMarkup :: (ToMarkup a) => a -> Markup
quoteMarkup element = toMarkup [quote, toMarkup element, quote]
  where quote = string "'"

-- | Formats a \"Doe\/John\" name as \"John Doe\" or \"John D.\".
formatPerson :: Bool -> Text -> Text
formatPerson abbrev name =
  let w = "/" `Text.splitOn` name
  in case w of
       [l, f] | not (Text.null l) ->
                if abbrev
                  then f <> " " `Text.snoc` Text.head l `Text.snoc` '.'
                  else f <> " " <> l
       _ -> name

folderLocations :: PicDir -> Text
folderLocations =
  Text.intercalate ", " . Map.keys . gExifLocations . pdExif

folderPeople :: PicDir -> Text
folderPeople =
  Text.intercalate ", " . map (formatPerson True) . Map.keys . gExifPeople . pdExif

folderKeywords :: PicDir -> Text
folderKeywords =
  Text.intercalate ", " . Map.keys . gExifKeywords . pdExif

buildTopNLenses :: Map.Map Text (LensInfo, (Int, FileOffset)) -> Int -> [(Int, FileOffset, Text, LensInfo)]
buildTopNLenses m n =
  let allItems = sortBy (flip compare) $
                 Map.foldlWithKey' (\a k (li, (cnt, sz)) ->
                                       (cnt, sz, k, li):a) [] m
      top10 = if length allItems > n
                then let t10 = reverse $ take (n-1) allItems
                         r  = drop (n-1) allItems
                         (rc, rs) = foldl' (\(c, s) (cnt, sz, _, _) ->
                                               (c+cnt, s+sz)) (0, 0) r
                     in (rc, rs, "Others", unknownLens): t10
              else allItems
  in top10

showLensAperture :: Maybe LensAperture -> String
showLensAperture Nothing = "?"
showLensAperture (Just (FixedAperture x)) =
  printf "f/%.01f (%.03f)" x x
showLensAperture (Just (VariableAperture min' max')) =
  printf "f/%.01f-%.01f (%.03f-%.03f)" min' max' min' max'

showLensFL :: Maybe LensFocalLength -> String
showLensFL Nothing = "?"
showLensFL (Just (Prime fl)) =
  printf "%.0fmm (%.03f)" fl fl
showLensFL (Just (Zoom min' max')) =
  printf "%.0f-%.0fmm (%.03f-%.03f)" min' max' min' max'
