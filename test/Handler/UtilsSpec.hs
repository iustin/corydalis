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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.UtilsSpec (spec) where

import           TestImport

import           Data.Aeson    (encode)
import           Data.Default  (def)
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Map      as Map
import qualified Data.Text.Short as TS
import           Data.Time     (CalendarDiffDays (..), LocalTime (..),
                                ZonedTime (..), midnight, utc)
import           Text.Blaze.Html.Renderer.Text (renderHtml)

import           Exif
import           Handler.Utils
import           Indexer       (Symbol (..))
import           Pics          (File (..), Image (..), Occurrence (..),
                                PicDir (..), RepoDirs, imageTimeKey)
import           Stats         (CameraInfo (..))

spec :: Spec
spec = do
  formatPeriodSpec
  plotTextSpec
  keeperRateSpec
  labelsSpec
  timeSpec
  lensSpec
  imageSpec
  statsSpec

keeperRateSpec :: Spec
keeperRateSpec = describe "formatKeeperRate" $ do
  it "formats the rate as a percentage" $
    formatKeeperRate 0.5 `shouldBe` "50.00%"
  it "keeps two decimals" $
    formatKeeperRate (1 / 3) `shouldBe` "33.33%"

plotTextSpec :: Spec
plotTextSpec = describe "chart text" $ do
  it "escapes the characters Plotly reads as markup" $ do
    escapePlotText "<a href=\"https://evil.example\">Canon</a>"
      `shouldBe` "&lt;a href=\"https://evil.example\"&gt;Canon&lt;/a&gt;"
    -- Ampersands first, so that entities show as written.
    escapePlotText "A&amp;B" `shouldBe` "A&amp;amp;B"
  it "keeps quotes, which Plotly wouldn't decode as entities" $
    escapePlotText "EF 70-200mm \"II\" 'L'" `shouldBe` "EF 70-200mm \"II\" 'L'"
  it "is escaped in the charts' data, but not the trusted parts" $ do
    let series = def { gdName = plotText "<b>Nikon</b>"
                     , gdX = Just [plotText "<i>Z</i>"]
                     , gdY = Just [1 :: Int]
                     , gdText = Just [PlotText "a&b"]
                     , gdExtra = [("hovertemplate", "%{label}<extra></extra>")]
                     } :: GraphData PlotText Int Int
        json = BSL.toStrict (encode series)
    json `shouldSatisfy` ("&lt;b&gt;Nikon&lt;/b&gt;" `isInfixOf`)
    json `shouldSatisfy` ("&lt;i&gt;Z&lt;/i&gt;" `isInfixOf`)
    json `shouldSatisfy` ("a&amp;b" `isInfixOf`)
    json `shouldSatisfy` ("%{label}<extra></extra>" `isInfixOf`)
    json `shouldNotSatisfy` ("<b>" `isInfixOf`)

formatPeriodSpec :: Spec
formatPeriodSpec = describe "formatPeriod" $ do
  it "counts both days" $
    formatPeriod (fromGregorian 2024 3 5) (fromGregorian 2024 3 5)
      `shouldBe` "1 day"
  it "uses weeks and days" $
    formatPeriod (fromGregorian 2024 3 1) (fromGregorian 2024 3 10)
      `shouldBe` "1 week and 3 days"
  it "uses calendar months" $ do
    -- February 2024 has 29 days, March 31; both are a month.
    formatPeriod (fromGregorian 2024 2 1) (fromGregorian 2024 2 29)
      `shouldBe` "1 month"
    formatPeriod (fromGregorian 2024 3 1) (fromGregorian 2024 3 31)
      `shouldBe` "1 month"
  it "uses calendar years, and keeps the two largest units" $ do
    formatPeriod (fromGregorian 2023 1 1) (fromGregorian 2023 12 31)
      `shouldBe` "1 year"
    formatPeriod (fromGregorian 2022 1 1) (fromGregorian 2024 3 20)
      `shouldBe` "2 years and 2 months"

labelsSpec :: Spec
labelsSpec = describe "labels" $ do
  let classes = [minBound .. maxBound] :: [FolderClass]
      distinct xs = length (ordNub xs) == length xs
  it "has distinct names for all folder classes" $ do
    map fcName classes `shouldSatisfy` distinct
    map fcShortName classes `shouldSatisfy` distinct
    map fcDescription classes `shouldSatisfy` distinct
    fcName FolderUnprocessed `shouldBe` "not fully processed"
    fcShortName FolderUnprocessed `shouldBe` "in progress"
  it "has icons for all folder classes" $ do
    forM_ classes $ \c -> fcIcon c `shouldNotBe` []
    fcIcon FolderProcessed `shouldBe` [iconRaw, iconProcessed, iconDone]
    fcIcon FolderUnprocessed `shouldBe` [iconRaw, iconProcessed, iconWIP]
    fcIcon FolderEmpty `shouldBe` [iconFolder]
  it "has icons for all image statuses" $ do
    isIcon ImageProcessed `shouldBe` [iconRaw, iconProcessed]
    isIcon ImageUnprocessed `shouldBe` [iconRaw]
    isIcon ImageStandalone `shouldBe` [iconProcessed]
    isIcon ImageOrphaned `shouldBe` [iconPlainFile]
  it "has a Font Awesome icon for all search symbols" $ do
    let icons = map atomIcon [minBound .. maxBound]
    forM_ icons (`shouldSatisfy` ("fa-" `isPrefixOf`))
    atomIcon TCamera `shouldBe` "fa-solid fa-camera"
  it "describes flash sources" $
    map formatFlashSource (Nothing : map Just [minBound .. maxBound])
      `shouldSatisfy` distinct
  it "formats unknown symbols" $ do
    formatMaybeSymbolUnknown Nothing `shouldBe` "unknown"
    formatMaybeSymbolUnknown (Just (mkSym "Nikon")) `shouldBe` "Nikon"
  it "quotes markup" $
    renderHtml (quoteMarkup ("a<b" :: Text)) `shouldBe` "&#39;a&lt;b&#39;"

timeSpec :: Spec
timeSpec = describe "time formatting" $ do
  let zoned = ZonedTime (LocalTime (fromGregorian 2024 3 5) midnight) utc
  it "formats local times" $
    showLocalTime zoned `shouldBe` "2024-03-05 00:00:00.0000 UTC"
  it "formats exif times" $
    showExifTime (ExifTime zoned) `shouldBe` "2024-03-05 00:00:00.0000 UTC"
  it "formats timestamps as UTC" $
    showTimestamp 86400 `shouldBe` "1970-01-02 00:00:00.0000 UTC"
  it "formats dates" $ do
    showLocalDate (fromGregorian 2024 3 5) `shouldBe` "2024-03-05"
    showLocalDateWithFormat "%Y" (fromGregorian 2024 3 5) `shouldBe` "2024"
  it "formats durations in days" $ do
    showDaysDuration (CalendarDiffDays 0 5) `shouldBe` "5 days"
    showDaysDuration (CalendarDiffDays 2 5) `shouldBe` "2 months and 5 days"
  it "formats missing file timestamps as empty" $ do
    showFileTimestamp Nothing `shouldBe` ""
    showFileLatestTS Nothing `shouldBe` ""
  it "formats file timestamps" $ do
    let f = (simpleFile "a.jpg") { fileMTime = 86400 }
    showFileTimestamp (Just f) `shouldBe` "1970-01-02 00:00:00.0000 UTC"
  it "formats rates and percentages" $ do
    formatPicRate 10 5 7 `shouldBe` "3.50"
    formatPercent 12.5 `shouldBe` Just "12.50"
    formatPercent (0 / 0) `shouldBe` Nothing
  it "formats sizes in binary units" $ do
    showBinary (1024 :: Int) `shouldBe` "1KiB"
    showBinary (2048 :: Int) `shouldBe` "2KiB"
    showBinary (0 :: Int) `shouldBe` "0B"

lensSpec :: Spec
lensSpec = describe "lens formatting" $ do
  it "formats apertures" $ do
    showLensAperture Nothing `shouldBe` "?"
    showLensAperture (Just (FixedAperture 2.8)) `shouldBe` "f/2.8 (2.800)"
    showLensAperture (Just (VariableAperture 3.5 5.6))
      `shouldBe` "f/3.5-5.6 (3.500-5.600)"
  it "formats focal lengths" $ do
    showLensFL Nothing `shouldBe` "?"
    showLensFL (Just (Prime 50)) `shouldBe` "50mm (50.000)"
    showLensFL (Just (Zoom 24 70)) `shouldBe` "24-70mm (24.000-70.000)"
  it "names the others lens" $
    liName lensOthers `shouldBe` mkSym "others"
  it "links to the lens page" $
    lensInfoRoute (lensOthers { liName = mkSym "50mm" }) `shouldBe` LensInfoR "50mm"

imageSpec :: Spec
imageSpec = withConfig $ describe "images" $ do
  it "formats the exif date of an image" $ \config -> do
    let img = simpleUntrackedImage config "f" "a"
        dated = img { imgExif = (imgExif img) { exifCreateDate = Just (exifDate 2024 3 5) } }
    showExifDate img `shouldBe` "?"
    showExifDate dated `shouldBe` "2024-03-05 00:00:00.0000 UTC"
  it "marks orphaned images" $ \config -> do
    let img = simpleUntrackedImage config "f" "a"
    imgRowClass img { imgStatus = ImageOrphaned } `shouldBe` "danger"
    imgRowClass img { imgStatus = ImageProcessed } `shouldBe` ""
  it "looks up images in a repository" $ \config -> do
    let img = simpleUntrackedImage config "f" "a"
        dirs = Map.singleton "f" ((createTestPicDir "f") { pdImages = Map.singleton "a" img }) :: RepoDirs
        repo = mkRepository dirs
    imgName <$> lookupImage repo "f" "a" `shouldBe` Just "a"
    imgName <$> lookupImage repo "f" "b" `shouldBe` Nothing
    imgName <$> lookupImage repo "g" "a" `shouldBe` Nothing
  it "builds resolution parameters" $ \config -> do
    let img = simpleUntrackedImage config "f" "a"
    makeResParam 100 `shouldBe` [(resolutionParam, "100")]
    imageBytesAtRes img 100 `shouldBe` (ImageBytesR "f" "a", [(resolutionParam, "100")])
  it "picks nothing at random from no images" $
    const $ randomPick Map.empty `shouldReturn` Nothing
  it "picks the only image at random" $ \config -> do
    let img = simpleUntrackedImage config "f" "a"
    picked <- randomPick (Map.singleton ("f", imageTimeKey img) img)
    imgName <$> picked `shouldBe` Just "a"
  it "builds the aperture and focal length chart" $ \config -> do
    let shot n fl aper = exifImage config "f" n $ \e ->
          e { exifFocalLength = fl, exifAperture = aper }
        imgs = [ shot "a" (Just 50) (Just 2.8), shot "b" (Just 50) (Just 2.8)
               , shot "c" (Just 35) (Just 4), shot "d" Nothing (Just 4) ]
    computeApFLStats imgs `shouldBe` Map.fromList [((35, 4), 1), ((50, 2.8), 2)]
    let json = toStrict (encode (buildLensApFL imgs))
    json `shouldSatisfy` ("50.0mm @ f/2.8: 2 images" `isInfixOf`)
    json `shouldSatisfy` ("\"ytickvals\":[2.8,4]" `isInfixOf`)

statsSpec :: Spec
statsSpec = describe "statistics" $ do
  let occ n = Occurrence { ocFiles = n, ocFileSize = fromIntegral n * 10
                         , ocFolders = 1, ocData = "d" <> tshow n
                         , ocTrends = Map.singleton (2024, 1) (fromIntegral n)
                         , ocDateRange = Nothing } :: Occurrence Text
      stats = Map.fromList [ ("a", occ 1), ("b", occ 5), ("c", occ 3), ("d", occ 2) ]
      names = map (\(_, _, k, _, _) -> k)
  it "keeps all items when under the limit" $
    names (buildTopNItems "others" stats 4) `shouldBe` ["b", "c", "d", "a"]
  it "groups the items over the limit as others" $ do
    let top = buildTopNItems "others" stats 3
    names top `shouldBe` ["Others", "c", "b"]
    case top of
      (cnt, sz, _, d, tr):_ -> do
        (cnt, sz, d) `shouldBe` (3, 30, "others")
        tr `shouldBe` Map.singleton (2024, 1) 3
      [] -> expectationFailure "No items"
  it "builds the camera and lens charts" $ do
    let json = toStrict . encode $
          buildCamLensStats "others" 2 2 TS.fromText TS.fromText
            stats
    json `shouldSatisfy` ("\"imagecount\"" `isInfixOf`)
    json `shouldSatisfy` ("\"trends\"" `isInfixOf`)
    json `shouldSatisfy` ("\"Others\"" `isInfixOf`)
    json `shouldSatisfy` ("2024-1" `isInfixOf`)
  it "counts items" $
    countItems ["a", "b", "a" :: Text] `shouldBe` [("a", 2), ("b", 1)]
  it "selects the top items, skipping unknown ones" $ do
    let ns = Map.fromList [(Nothing, 100), (Just "a", 1), (Just "b", 3), (Just "c", 2)]
               :: Map (Maybe Text) Integer
    topN 2 ns `shouldBe` (["b", "c"], ["a"])
  it "builds search parameters" $ do
    atomAsParam TCamera (Just "Nikon") `shouldBe` ("camera", "Nikon")
    atomAsParam TCamera Nothing `shouldBe` ("no-camera", "")
  it "has the camera info default" $
    ciName (def :: CameraInfo) `shouldBe` mkSym "unknown"
