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
import           Utils.Parsing         (parseDecimal)

import           Data.Aeson.Key        (fromText)
import qualified Data.Map              as Map
import           Data.Prefix.Units
import qualified Data.Text             as Text
import qualified Data.Text.Lazy        as TL
import           Data.Time
import           Data.Time.Clock.POSIX
import           System.Random         (getStdRandom, randomR)
import           Text.Blaze            (Markup, ToMarkup, string, toMarkup)


-- | Formats a double as a percent value. NaN values are transformed
-- into a Nothing.
formatPercent :: Double -> Maybe TL.Text
formatPercent v | isNaN v = Nothing
                | otherwise = Just $ format (fixed 2) v

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

isIcon :: ImageStatus -> [Text]
isIcon ImageOrphaned    = [iconPlainFile]
isIcon ImageStandalone  = [iconProcessed]
isIcon ImageUnprocessed = [iconRaw]
isIcon ImageProcessed   = [iconRaw, iconProcessed]

iconRaw :: Text
iconRaw = "fa-solid fa-file-image"

iconProcessed :: Text
iconProcessed = "far fa-file-image"

iconPlainFile :: Text
iconPlainFile = "far fa-file-alt"

iconMovie :: Text
iconMovie = "fa-solid fa-video"

iconDone :: Text
iconDone = "fa-solid fa-check"

iconWIP :: Text
iconWIP = "fa-solid fa-hourglass-half"

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
atomIcon TCountry      = "fa-solid fa-globe"
atomIcon TProvince     = "fa-solid fa-map"
atomIcon TCity         = "fa-solid fa-building"
atomIcon TLocation     = "fa-solid fa-map-signs"
atomIcon TPerson       = "fa-solid fa-users"
atomIcon TKeyword      = "fa-solid fa-tags"
atomIcon TTitle        = "fa-solid fa-comment"
atomIcon TCaption      = "fa-solid fa-comment-dots"
atomIcon TYear         = "fa-solid fa-calendar"
atomIcon TSeason       = "fa-solid fa-calendar"
atomIcon TMonth        = "fa-solid fa-calendar"
atomIcon TDay          = "fa-solid fa-calendar"
atomIcon TCamera       = "fa-solid fa-camera"
atomIcon TLens         = "fa-solid fa-camera" -- :(
atomIcon TFStop        = "far fa-circle" -- FIXME: check font-awesome sometimes
atomIcon TShutterSpeed = "fa-solid fa-stopwatch"
atomIcon TIso          = "fa-solid fa-cloud-sun-rain"
atomIcon TFocalLength  = "fa-solid fa-ruler-horizontal" -- FIXME: sigh…
atomIcon TProblem      = "fa-solid fa-bomb"
atomIcon TType         = "fa-solid fa-file"
atomIcon TFolder       = "fa-solid fa-folder"
atomIcon TFileName     = "fa-solid fa-file"
atomIcon TStatus       = "fa-solid fa-file-alt"
atomIcon TFClass       = "fa-solid fa-folder"
atomIcon TRating       = "fa-solid fa-star"
atomIcon TPplCnt       = "fa-solid fa-users"
atomIcon TKwdCnt       = "fa-solid fa-tags"
atomIcon TFlashSrc     = "fa-solid fa-bolt"
atomIcon TFlashMode    = "fa-solid fa-bolt" -- FIXME - maybe multi-symbol?
atomIcon TMegapixels   = "fa-solid fa-chart-area"

dataMasonry :: Text
dataMasonry   = "{\"percentPosition\": true}"

showTimestamp :: NominalDiffTime -> Text
showTimestamp =
  showLocalTime . utcToZonedTime utc . posixSecondsToUTCTime

showLocalTime :: ZonedTime -> Text
showLocalTime =
  Text.pack . formatTime defaultTimeLocale "%F %T%4Q %EZ"

showExifTime :: ExifTime -> Text
showExifTime = showLocalTime . etTime

showLocalDateWithFormat :: (FormatTime t) => String -> t -> Text
showLocalDateWithFormat fmt =
  Text.pack . formatTime defaultTimeLocale fmt

showLocalDate :: (FormatTime t) => t -> Text
showLocalDate = showLocalDateWithFormat "%F"

-- | Formats a duration in days as a human-readable string.
showDaysDuration :: CalendarDiffDays -> Text
showDaysDuration dd@(CalendarDiffDays m _) =
  Text.pack $ formatTime defaultTimeLocale fmt dd
  where fmt = if m > 0
              then "%b months and %d days"
              else "%d days"

showExifDate :: Image -> Text
showExifDate =
  maybe "?" showExifTime . exifCreateDate . imgExif

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

getFolderImage :: PicDir -> ImageName -> Handler Image
getFolderImage dir iname =
  maybe notFound return $ Map.lookup iname (pdImages dir)

getImage :: Text -> ImageName -> Handler Image
getImage folder iname = do
  dir <- getFolder folder
  getFolderImage dir iname

lookupImage :: Repository -> Text -> ImageName -> Maybe Image
lookupImage (repoDirs -> pics) folder image = do
  dir <- Map.lookup folder pics
  Map.lookup image (pdImages dir)

-- | Quotes content such that copy-paste is easier if within a span.
quoteMarkup :: (ToMarkup a) => a -> Markup
quoteMarkup element = toMarkup [quote, toMarkup element, quote]
  where quote = Text.Blaze.string "'"

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
  format ("f/" % fixed 1 % " (" <> fixed 3 % ")") f
showLensAperture (Just (VariableAperture min' max')) =
  format
    ("f/" % fixed 1 % "-" % fixed 1 % " (" % fixed 3 % "-" % fixed 3 % ")")
    min' max' min' max'

showLensFL :: Maybe LensFocalLength -> TL.Text
showLensFL Nothing = "?"
showLensFL (Just (Prime fl)) =
  format (fixed 0 % "mm (" <> fixed 3 % ")") fl
showLensFL (Just (Zoom min' max')) =
  format (fixed 0 % "-" % fixed 0 % "mm (" % fixed 3 % "-" % fixed 3 % ")")
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

resolutionParam :: Text
resolutionParam = "res"

makeResParam :: Int -> [(Text, Text)]
makeResParam res = [(resolutionParam, sformat shown res)]

getResParam :: Handler (Maybe ImageSize)
getResParam = do
  res <- lookupGetParam resolutionParam
  case res of
    Nothing -> return Nothing
    Just v -> case parseDecimal v of
      Left msg   -> invalidArgs [msg]
      Right res' -> return . Just . ImageSize $ res'

-- | Helper to build an image bytes URL for a specific resolution.
imageBytesAtRes :: Image -> Int -> (Route App, [(Text, Text)])
imageBytesAtRes Image{imgParent = folder, imgName = iname} res =
  (ImageBytesR folder iname, makeResParam res)

setHtmlTitle :: Text -> Widget
setHtmlTitle = setTitle . toHtml . ("Corydalis: " <>)

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
             ] ++ map (\(a, b) -> fromText a .= b) gdExtra

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
        sformat (fixed 1 % "mm @ f/" % fixed 1 % ": " % int % " images")
        fl' ap' cnt'
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

-- | Builds statistics for UI display from occurrence data.
-- Takes a default value for the "others" category, limits for top items to display
-- in bar chart and trend chart, accessor functions for item names, and a map of occurrences.
-- Returns a JSON object with two fields: "imagecount" (bar chart data) and "trends" (trend lines).
buildCamLensStats :: Ord a
                  => a                       -- ^ Default value for "others" category
                  -> Int                     -- ^ Limit for number of items to display in bar chart
                  -> Int                     -- ^ Limit for number of items to display in trend chart
                  -> (a -> Text)             -- ^ Function to extract item name for bar chart
                  -> (a -> Text)             -- ^ Function to extract item name for trend chart
                  -> Map Text (Occurrence a) -- ^ Map of camera/lens occurrences
                  -> Value                   -- ^ JSON object with "imagecount" and "trends"
buildCamLensStats others n1 n2 nameFn1 nameFn2 stats =
  let top1 = buildTopNItems others stats n1
      top2 = buildTopNItems others stats n2
      jsonl = foldl' (\a (cnt, _, k, li, _) ->
                        def { gdName = nameFn1 li
                            , gdType = "bar"
                            , gdMode = Just "markers"
                            , gdX = Just [k]
                            , gdY = Just [fromIntegral cnt]
                            , gdExtra = [("hovertemplate", "%{label}: %{y}<extra></extra>")]
                            }:a)
              ([]::[GraphData Text Int64 Int64]) top1
      jsont = foldl' (\a (_, _, _, li, tr) ->
                        let (d, c) = unzip $ Map.assocs tr
                            d' = map (uncurry $ sformat (int % "-" % int)) d
                            c' = map fromIntegral c
                        in
                        def { gdName = nameFn2 li
                            , gdType = "bar"
                            , gdX = Just d'
                            , gdY = Just c'
                            , gdExtra = [("hovertemplate", "%{fullData.name}: %{value}<extra>%{label}</extra>")]
                            }:a)
              ([]::[GraphData Text Int64 Int64]) top2
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

-- | Parses the parameters to the (real) handler and build search-related
-- attributes.
searchContext :: Handler (Ctx, Config, [(Text, Text)], Atom, Text, Repository)
searchContext = do
  ctx <- getContext
  (_, atom) <- getAtomParams
  let search_string = atomDescription atom
  pics <- getPics
  return (ctx, ctxConfig ctx, atomToParams atom, atom, search_string, pics)

-- | Gets the top N and the rest of non-"unknown" items from a
-- NameStats.
topN :: Int -> NameStats a -> ([a], [a])
topN n =
  splitAt n .
  map fst .
  sortBy (flip compare `on` snd) .
  foldl' (\l (a, b) ->
             case a of
               Nothing -> l
               Just a' -> (a', b):l
         ) [] .
  Map.toList

formatPicRate :: Integer -> Integer -> Integer -> Text
formatPicRate days pics period =
  let ddays = fromIntegral days::Double
      dpics = fromIntegral pics
      dperiod = fromIntegral period
      rate = dpics * dperiod / ddays
  in sformat (fixed 2) rate

formatFlashSource :: Maybe FlashSource -> Text
formatFlashSource f =
  case f of
    Nothing                  -> "unknown"
    Just FlashSourceNone     -> "no flash used"
    Just FlashSourceInternal -> "camera internal flash"
    Just FlashSourceExternal -> "external flash"

-- | Pick a random image from a list of images.
randomPick :: Map (Text, ImageTimeKey) Image -> IO (Maybe Image)
randomPick images = do
  if Map.null images
    then return Nothing
    else do
      idx <- getStdRandom $ randomR (0, Map.size images - 1)
      -- This _should_ be safe, since idx in the right range. If not,
      -- well...
      return . Just . snd . Map.elemAt idx $ images
