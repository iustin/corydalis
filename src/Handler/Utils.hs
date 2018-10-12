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
import           Indexer
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

fcShortName :: FolderClass -> Text
fcShortName FolderRaw         = "raw"
fcShortName FolderStandalone  = "standalone"
fcShortName FolderUnprocessed = "in progress"
fcShortName FolderProcessed   = "processed"
fcShortName FolderEmpty       = "empty"
fcShortName FolderMixed       = "mixed"

isShortName :: ImageStatus -> Text
isShortName ImageOrphaned   = "orphaned"
isShortName ImageStandalone = "standalone"
isShortName ImageRaw        = "unprocessed"
isShortName ImageProcessed  = "processed"

isIcon :: ImageStatus -> [Text]
isIcon ImageOrphaned   = [iconPlainFile]
isIcon ImageStandalone = [iconProcessed]
isIcon ImageRaw        = [iconRaw]
isIcon ImageProcessed  = [iconRaw, iconProcessed]

iconRaw :: Text
iconRaw = "fas fa-file-image"

iconProcessed :: Text
iconProcessed = "far fa-file-image"

iconPlainFile :: Text
iconPlainFile = "far fa-file-alt"

iconMovie :: Text
iconMovie = "fas fa-video"

iconDone :: Text
iconDone = "fas fa-check"

iconWIP :: Text
iconWIP = "fas fa-hourglass-half"

iconFolder :: Text
iconFolder = "far fa-folder"

fcIcon :: FolderClass -> [Text]
fcIcon FolderRaw         = [iconRaw]
fcIcon FolderStandalone  = [iconProcessed]
fcIcon FolderUnprocessed = [iconRaw, iconProcessed, iconWIP]
fcIcon FolderProcessed   = [iconRaw, iconProcessed, iconDone]
fcIcon FolderEmpty       = [iconFolder]
fcIcon FolderMixed       = [iconProcessed, iconRaw, iconProcessed]

fcDescription :: FolderClass -> Text
fcDescription FolderRaw         = "contains only unprocessed RAW files"
fcDescription FolderStandalone  = "contains only files without a RAW format"
fcDescription FolderUnprocessed = "contains unprocessed RAW files (and possibly \
                                   \standalone files)"
fcDescription FolderProcessed   = "contains RAW files, all processed"
fcDescription FolderEmpty       = "contains no image files"
fcDescription FolderMixed       = "contains both RAW files (processed) \
                                  \and files without RAW storage"

atomIcon :: Symbol -> Text
atomIcon TCountry  = "fas fa-globe"
atomIcon TProvince = "fas fa-map"
atomIcon TCity     = "fas fa-building"
atomIcon TLocation = "fas fa-map-signs"
atomIcon TPerson   = "fas fa-users"
atomIcon TKeyword  = "fas fa-tags"
atomIcon TYear     = "fas fa-calendar"
atomIcon TCamera   = "fas fa-camera"
atomIcon TLens     = "fas fa-camera" -- :(
atomIcon TProblem  = "fas fa-bomb"
atomIcon TType     = "fas fa-file"
atomIcon TPath     = "fas fs-folder"

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

folderLocations :: PicDir -> Text
folderLocations =
  Text.intercalate ", " . catMaybes . Map.keys . gExifLocations . pdExif

folderPeople :: PicDir -> Text
folderPeople =
  Text.intercalate ", " . map (formatPerson True) . catMaybes . Map.keys . gExifPeople . pdExif

folderKeywords :: PicDir -> Text
folderKeywords =
  Text.intercalate ", " . catMaybes . Map.keys . gExifKeywords . pdExif

buildTopNLenses :: Map.Map Text (LensInfo, Occurrence) -> Int -> [(Integer, FileOffset, Text, LensInfo)]
buildTopNLenses m n =
  let allItems = sortBy (flip compare) $
                 Map.foldlWithKey' (\a k (li, Occurrence cnt sz _) ->
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

countItems :: (Ord a) => [a] -> [(a, Int64)]
countItems =
  Map.toList .
  foldl' (\m c -> Map.insertWith (+) c 1 m) Map.empty

getParams :: Handler [(Text, Text)]
getParams = reqGetParams <$> getRequest

getAtomParams :: Handler (UrlParams, Atom)
getAtomParams = do
  params <- getParams
  atom <- case parseAtomParams params of
            Right v  -> return v
            Left msg -> invalidArgs [msg]
  return (params, atom)

getAtomAndSearch :: Handler (UrlParams, Atom, SearchResults)
getAtomAndSearch = do
  (_, atom) <- getAtomParams
  pics <- getPics
  images <- liftIO $ searchImages atom pics
  return (atomToParams atom, atom, images)

atomAsParam :: Symbol -> Maybe Text -> (Text, Text)
atomAsParam s (Just t) = (symbolName s, t)
atomAsParam s Nothing  = (negSymbolName s, "")

setHtmlTitle :: Text -> Widget
setHtmlTitle = setTitle . toHtml . ("Corydalis: " <>)

addPlotly :: Widget
addPlotly = addScript (StaticR plotly_js_plotly_js)
