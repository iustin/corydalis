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
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ViewPatterns      #-}

module Handler.Utils where

import           Exif
import           Import
import           Indexer
import           Pics

import qualified Data.Map                   as Map
import           Data.Prefix.Units
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as TL
import           Data.Time
import           Data.Time.Clock.POSIX
import           Formatting                 hiding (string)
import qualified Formatting.ShortFormatters as F
import           Text.Blaze                 (Markup, ToMarkup, string, toMarkup)

-- | Formats a double as a percent value. NaN values are transformed
-- into a Nothing.
formatPercent :: Double -> Maybe TL.Text
formatPercent v | isNaN v = Nothing
                | otherwise = Just $ format (F.f 2) v

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
atomIcon TPath     = "fas fa-folder"
atomIcon TStatus   = "fas fa-file-alt"

showTimestamp :: NominalDiffTime -> Text
showTimestamp =
  showLocalTime . utcToLocalTime utc . posixSecondsToUTCTime

showLocalTime :: LocalTime -> Text
showLocalTime ts =
  Text.pack $ ft ++ pico
    where pico = take 4 $ formatTime defaultTimeLocale "%q" ts
          ft = formatTime defaultTimeLocale "%F %T." ts

showLocalDate :: LocalTime -> Text
showLocalDate=
  Text.pack . formatTime defaultTimeLocale "%F"

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
  ctx <- getContext
  liftIO $ launchScanFileSystem ctx

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

buildTopNItems :: (Ord a)
               => a -> Map.Map Text (Occurrence a) -> Int -> [(Integer, FileOffset, Text, a, Trends)]
buildTopNItems d m n =
  let allItems = sortBy (flip compare) $
                 Map.foldlWithKey' (\a k (Occurrence cnt sz _ li tr _) ->
                                       (cnt, sz, k, li, tr):a) [] m
      top10 = if length allItems > n
                then let t10 = reverse $ take (n-1) allItems
                         r  = drop (n-1) allItems
                         (rc, rs, tr) =
                           foldl' (\(c, s, t) (cnt, sz, _, _, tr') ->
                                      (c+cnt, s+sz, Map.unionWith (+) t tr'))
                             (0, 0, Map.empty) r
                     in (rc, rs, "Others", d, tr): t10
              else allItems
  in top10

showLensAperture :: Maybe LensAperture -> TL.Text
showLensAperture Nothing = "?"
showLensAperture (Just (FixedAperture f)) =
  format ("f/" % F.f 1 % " (" <> F.f 3 % ")") f
showLensAperture (Just (VariableAperture min' max')) =
  format ("f/" % F.f 1 % "-" % F.f 1 % " (" % F.f 3 % "-" % F.f 3 % ")")
    min' max' min' max'

showLensFL :: Maybe LensFocalLength -> TL.Text
showLensFL Nothing = "?"
showLensFL (Just (Prime fl)) =
  format (F.f 0 % "mm (" <> F.f 3 % ")") fl
showLensFL (Just (Zoom min' max')) =
  format (F.f 0 % "-" % F.f 0 % "mm (" % F.f 3 % "-" % F.f 3 % ")")
    min' max' min' max'

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
  ctx <- getContext
  images <- liftIO $ searchImages ctx atom pics
  return (atomToParams atom, atom, images)

atomAsParam :: Symbol -> Maybe Text -> (Text, Text)
atomAsParam s (Just t) = (symbolName s, t)
atomAsParam s Nothing  = (negSymbolName s, "")

setHtmlTitle :: Text -> Widget
setHtmlTitle = setTitle . toHtml . ("Corydalis: " <>)

addPlotly :: Widget
addPlotly = addScript (StaticR plotly_js_plotly_cartesian_js)

counterOne :: Int64
counterOne = 1

data GraphData a b c = GraphData
  { gdName  :: Text
  , gdType  :: Text
  , gdX     :: Maybe [a]
  , gdY     :: Maybe [b]
  , gdZ     :: Maybe [c]
  , gdText  :: Maybe [Text]
  , gdMode  :: Maybe Text
  , gdExtra :: [(Text, Value)]
  }

instance Default (GraphData a b c) where
  def = GraphData { gdName = ""
                  , gdType = "scatter"
                  , gdX = Nothing
                  , gdY = Nothing
                  , gdZ = Nothing
                  , gdText = Nothing
                  , gdMode = Nothing
                  , gdExtra = []
                  }

instance (ToJSON a, ToJSON b, ToJSON c) => ToJSON (GraphData a b c) where
  toJSON GraphData {..} =
    object $ [ "name"  .= gdName
             , "type"  .= gdType
             , "x"     .= gdX
             , "y"     .= gdY
             , "z"     .= gdZ
             , "text"  .= gdText
             , "mode"  .= gdMode
             ] ++ map (uncurry (.=)) gdExtra

computeApFLStats :: [Image] -> Map.Map (Double, Double) Int64
computeApFLStats =
  foldl' (\m i ->
             let e = imgExif i
                 fl = exifFocalLength e
                 aper = exifAperture e
             in case (fl, aper) of
                  (Just fl', Just aper') -> Map.insertWith (+) (fl', aper') counterOne m
                  _ -> m
         ) Map.empty

buildLensApFL :: [Image] -> Value
buildLensApFL images =
  let faml = Map.toList $ computeApFLStats images
      (xys, cnt) = unzip faml
      maxCnt = fromIntegral $ maybe 5 maximum $ fromNullable cnt
      (x, y) = unzip xys
      allApertures = sort y
      hoverFmt ((fl', ap'), cnt') =
        show fl'++"mm @ f/" ++ show ap' ++ ": " ++ show cnt' ++ " images"
      jsonl = def { gdName = ""
                  , gdType = "scatter"
                  , gdMode = Just "markers"
                  , gdX = Just x
                  , gdY = Just y
                  , gdExtra = [ ("colorscale", "YIGnBu")
                              , ("reversescale", Bool True)
                              , ("marker", object [ "size" .= (toJSON cnt::Value)
                                                  , "sizemin" .= toJSON (4::Int)
                                                  , "sizemode" .= String "area"
                                                  , "sizeref" .= toJSON (2.0 * maxCnt / (90 ** 2)::Double)
                                                  ])
                              , ("text", toJSON $ map hoverFmt faml)
                              , ("hoverinfo", "text")
                              ]
                  } :: GraphData Double Double Double
  in object [ "lensflap"  .= [jsonl]
            , "ytickvals" .= allApertures
            , "yticktext" .= map show allApertures
            ]

buildCamLensStats :: Ord a
                  => a -> Int -> Int -> (a -> Text) -> (a -> Text)
                  -> Map Text (Occurrence a) -> Value
buildCamLensStats others n1 n2 nameFn1 nameFn2 stats =
  let top1 = buildTopNItems others stats n1
      top2 = buildTopNItems others stats n2
      jsonl = foldl' (\a (cnt, _, k, li, _) ->
                        def { gdName = nameFn1 li
                            , gdType = "bar"
                            , gdMode = Just "markers"
                            , gdX = Just [k]
                            , gdY = Just [fromIntegral cnt]
                            }:a)
              ([]::[GraphData Text Int64 Int64]) top1
      jsont = foldl' (\a (_, _, _, li, tr) ->
                        let (d, c) = unzip $ Map.assocs tr
                            d' = map (\(y, m) -> show y ++ "-" ++ show m) d
                            c' = map fromIntegral c
                        in
                        def { gdName = nameFn2 li
                            , gdType = "scatter"
                            , gdMode = Just "lines+markers"
                            , gdX = Just d'
                            , gdY = Just c'
                            , gdExtra = [ ("connectgaps", toJSON False)
                                        , ("stackgroup", toJSON ("one"::String))
                                        ]
                            }:a)
              ([]::[GraphData String Int64 Int64]) top2
  in object [ "imagecount" .= jsonl
            , "trends"     .= jsont
            ]

formatDate :: Integer -> TL.Text
formatDate d =
  let (years, d1) = d `quotRem` 365
      (months, d2) = d1 `quotRem` 30
      (weeks, days) = d2 `quotRem` 7
      pl :: Integer -> TL.Text
      pl n = if n > 1 then "s" else ""
      fpl :: Integer -> TL.Text -> Maybe TL.Text
      fpl n t = if n > 0
                then Just (format (int % " " % text % text) n t (pl n))
                else Nothing
      elems = [ fpl years "year"
              , fpl months "month"
              , fpl weeks "week"
              , fpl days "day"
              ]
  in TL.intercalate " and " $ take 2 $ catMaybes elems

-- | Select best movie for an image.
bestMovie :: Image -> Maybe File
bestMovie img =
  case imgMovs img of
    m:_ -> Just m
    _   -> imgMasterMov img
