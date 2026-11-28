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
{-# OPTIONS_GHC -Wno-orphans #-}

module ExifSpec (spec) where

import           Exif
import           TestImport

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Data.Aeson.Types    (Parser, parseMaybe)
import           Data.Either         (isLeft)
import           Data.Text.Arbitrary ()
import           Data.Time           (LocalTime (..), TimeOfDay (..),
                                      ZonedTime (..), TimeZone (..),
                                      zonedTimeToLocalTime)
import           Test.QuickCheck

-- Arbitrary instances for testing

instance Arbitrary Orientation where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary FlashSource where
  arbitrary = chooseEnum (minBound, maxBound)

spec :: Spec
spec = parallel $ do
  describe "ISO parsing" $ do
    let iso = parseMaybe parseValOrList
    it "parses numbers" $
      iso (Number 100) `shouldBe` Just 100
    it "parses single-element lists" $ do
      iso (toJSON [200 :: Int]) `shouldBe` Just 200
      parseMaybe (parseSingletonList :: Value -> Parser Int) (toJSON [1, 2 :: Int])
        `shouldBe` Nothing
    it "parses the string formats" $ do
      iso (String "400") `shouldBe` Just 400
      iso (String "ISO 800") `shouldBe` Just 800
      -- As written by some old Android versions.
      iso (String "1600, 0, 0") `shouldBe` Just 1600
    it "rejects other strings" $ do
      iso (String "fast") `shouldBe` Nothing
      iso (String "100x") `shouldBe` Nothing
      iso (String "a b c") `shouldBe` Nothing
      iso (String "") `shouldBe` Nothing
    it "rejects other values" $
      iso (Bool True) `shouldBe` Nothing
    it "reads integers without leftovers" $ do
      parseMaybe readInt "42" `shouldBe` Just 42
      parseMaybe readInt "42a" `shouldBe` Nothing
      parseMaybe readInt "a" `shouldBe` Nothing
      parseMaybe parseIsoString (Number 1) `shouldBe` Nothing

  withConfig $ describe "parsing exiftool output" $ do
    let parse config json = case parseExifs json of
          Left err -> Left ("invalid JSON: " <> err)
          Right [Right raw] -> Right (exifFromRaw config raw)
          Right [Left fre] -> Left (freMessage fre)
          Right other -> Left ("unexpected results: " <> tshow (length other))
        sym = mkSym
        localDate = fmap (zonedTimeToLocalTime . etTime) . exifCreateDate
    it "parses the exif information" $ \config -> do
      let json = "[{\"SourceFile\": \"a.nef\",\
                 \ \"Model\": {\"val\": \"D850\"},\
                 \ \"SerialNumber\": {\"val\": 1234},\
                 \ \"LensID\": {\"val\": \"AF-S 24-70\"},\
                 \ \"LensSpec\": {\"val\": \"24-70mm f/2.8\"},\
                 \ \"MinFocalLength\": {\"val\": \"24.0 mm\", \"num\": 24},\
                 \ \"MaxFocalLength\": {\"val\": \"70.0 mm\", \"num\": 70},\
                 \ \"MaxApertureAtMinFocal\": {\"num\": 2.8},\
                 \ \"MaxApertureAtMaxFocal\": {\"num\": 2.8},\
                 \ \"LensSerialNumber\": {\"val\": \"\"},\
                 \ \"HierarchicalSubject\": {\"val\": [\"_People|Ann\", \"Places|Paris\", \"_Ignored|x\"]},\
                 \ \"PersonInImage\": {\"val\": [\"Bob\"]},\
                 \ \"Country\": {\"val\": \"France\"},\
                 \ \"State\": {\"val\": \"IdF\"},\
                 \ \"City\": {\"val\": \"\"},\
                 \ \"Location\": {\"val\": \"Louvre\"},\
                 \ \"DateTimeOriginal\": {\"val\": \"2024:03:05 10:11:12\"},\
                 \ \"ISO\": {\"val\": \"200, 0, 0\"},\
                 \ \"ShutterSpeed\": {\"val\": \"1/100\", \"num\": 0.01},\
                 \ \"ShutterCount\": {\"val\": 20000000},\
                 \ \"Orientation\": {\"num\": 6},\
                 \ \"ImageWidth\": {\"num\": 0}\
                 \}]"
      case parse config json of
        Left err -> expectationFailure (unpack err)
        Right e -> do
          exifCamera e `shouldBe` Just (sym "D850 (#1234)")
          exifSerial e `shouldBe` Just (sym "1234")
          liName (exifLens e) `shouldBe` sym "AF-S 24-70"
          liSpec (exifLens e) `shouldBe` sym "24-70mm f/2.8"
          liFL (exifLens e) `shouldBe` Just (Zoom 24 70)
          liAp (exifLens e) `shouldBe` Just (FixedAperture 2.8)
          liSerial (exifLens e) `shouldBe` Nothing
          exifPeople e `shouldBe` setFromList [sym "Ann", sym "Bob"]
          exifKeywords e `shouldBe` setFromList [sym "Places", sym "Paris", sym "x"]
          exifCountry e `shouldBe` Just (sym "France")
          exifProvince e `shouldBe` Just (sym "IdF")
          exifCity e `shouldBe` Nothing
          exifLocation e `shouldBe` Just (sym "Louvre")
          localDate e `shouldBe`
            Just (LocalTime (fromGregorian 2024 3 5) (TimeOfDay 10 11 12))
          exifISO e `shouldBe` Just 200
          exifSSpeedDesc e `shouldBe` Just (sym "1/100")
          exifSSpeedVal e `shouldBe` Just 0.01
          exifShutterCount e `shouldBe` Nothing
          exifOrientation e `shouldBe` OrientationRightTop
          exifWidth e `shouldBe` Nothing
          exifWarning e `shouldBe` setFromList
            [ "Empty city information"
            , "Unlikely shutter count: 20000000"
            , "Invalid (Exif) image width 0"
            ]
    it "parses prime lenses, and serial numbers without a model" $ \config -> do
      let json = "[{\"SourceFile\": \"a.nef\",\
                 \ \"SerialNumber\": {\"val\": \"S1\"},\
                 \ \"LensModel\": {\"val\": \"50mm\"},\
                 \ \"MinFocalLength\": {\"num\": 50},\
                 \ \"MaxFocalLength\": {\"num\": 50},\
                 \ \"MaxApertureAtMinFocal\": {\"num\": 1.8},\
                 \ \"MaxApertureAtMaxFocal\": {\"num\": 2},\
                 \ \"LensSerialNumber\": {\"val\": 98765},\
                 \ \"CreateDate\": {\"val\": \"2024:03:05 10:11:12.5+02:00\"},\
                 \ \"ISO\": {\"val\": [100]}\
                 \}]"
      case parse config json of
        Left err -> expectationFailure (unpack err)
        Right e -> do
          exifCamera e `shouldBe` Just (sym "#S1")
          liName (exifLens e) `shouldBe` sym "50mm"
          liFL (exifLens e) `shouldBe` Just (Prime 50)
          liAp (exifLens e) `shouldBe` Just (VariableAperture 1.8 2)
          liSerial (exifLens e) `shouldBe` Just (sym "98765")
          fmap (zonedTimeToLocalTime . etTime) (exifCreateDate e) `shouldBe`
            Just (LocalTime (fromGregorian 2024 3 5) (TimeOfDay 10 11 12.5))
          fmap (timeZoneMinutes . zonedTimeZone . etTime) (exifCreateDate e)
            `shouldBe` Just 120
          exifISO e `shouldBe` Just 100
    it "reports the errors from exiftool" $ \config ->
      parse config "[{\"SourceFile\": \"b.jpg\", \"Error\": {\"val\": \"File not found\"}}]"
        `shouldBe` Left "File not found"
    it "reports invalid values" $ \config -> do
      parse config "[{\"SourceFile\": \"b.jpg\", \"Orientation\": {\"num\": 9}}]"
        `shouldSatisfy` isLeft
      parse config "[{\"SourceFile\": \"b.jpg\", \"Orientation\": {\"num\": 1.5}}]"
        `shouldSatisfy` isLeft
    it "skips results without a file name" $ \config ->
      parse config "[{\"Model\": {\"val\": \"D850\"}}]"
        `shouldBe` Left "unexpected results: 0"
    it "reports invalid JSON" $ \config ->
      parse config "not json" `shouldSatisfy` isLeft

  describe "JSON encoding" $ do
    it "encodes focal lengths" $ do
      toJSON (Prime 50) `shouldBe` object ["type" .= ("prime" :: Text), "focal" .= (50 :: Double)]
      toJSON (Zoom 24 70) `shouldBe`
        object [ "type" .= ("zoom" :: Text), "minFocal" .= (24 :: Double)
               , "maxFocal" .= (70 :: Double)]
    it "encodes apertures" $ do
      toJSON (FixedAperture 2) `shouldBe`
        object ["type" .= ("fixed" :: Text), "aperture" .= (2 :: Double)]
      toJSON (VariableAperture 3.5 5.6) `shouldBe`
        object [ "type" .= ("variable" :: Text), "minAperture" .= (3.5 :: Double)
               , "maxAperture" .= (5.6 :: Double)]
    it "encodes lens information" $ do
      let lens = unknownLens { liFL = Just (Prime 50), liSerial = Just (mkSym "123") }
          field k = parseMaybe (withObject "lens" (.: k)) (toJSON lens) :: Maybe Value
      field "name" `shouldBe` Just "unknown"
      field "focalLength" `shouldBe` Just (toJSON (Prime 50))
      field "aperture" `shouldBe` Just Null
      field "serial" `shouldBe` Just "123"
    it "encodes flash information" $
      toJSON (FlashInfo (Just FlashSourceInternal) (Just (mkSym "auto"))) `shouldBe`
        object ["source" .= (2 :: Int), "mode" .= ("auto" :: Text)]
    it "encodes exif times" $
      toJSON (exifDate 2024 3 5) `shouldBe` String "2024-03-05 00:00:00+0000"
    it "encodes exif information" $ do
      let exif = (def :: Exif) { exifCamera = Just (mkSym "Nikon")
                               , exifISO = Just 100
                               , exifCreateDate = Just (exifDate 2024 3 5) }
          field k = parseMaybe (withObject "exif" (.: k)) (toJSON exif) :: Maybe Value
      field "camera" `shouldBe` Just "Nikon"
      field "iso" `shouldBe` Just (Number 100)
      field "createDate" `shouldBe` Just "2024-03-05 00:00:00+0000"
      field "title" `shouldBe` Just Null
      field "lens" `shouldBe` Just (toJSON unknownLens)

  describe "Orientation JSON roundtrip" $ do
    -- Orientation has both ToJSON and FromJSON
    prop "toJSON/fromJSON roundtrip" $ \orientation ->
      decode (encode orientation) === Just (orientation :: Orientation)

    it "checks specific values" $ do
      decode (encode OrientationTopLeft) `shouldBe` Just OrientationTopLeft
      decode (encode OrientationRightBot) `shouldBe` Just OrientationRightBot

  describe "FlashSource JSON roundtrip" $ do
    prop "toJSON/fromJSON conversion matches parseFlashSource" $ \flashSource ->
      let encoded = encode flashSource
          asNumber = decode encoded :: Maybe Int
      in case asNumber of
           Just n  -> parseFlashSource n === Just (flashSource :: FlashSource)
           Nothing -> counterexample "Failed to decode as number" False

    it "checks specific mappings" $ do
      decode (encode FlashSourceNone) `shouldBe` Just (0 :: Int)
      decode (encode FlashSourceExternal) `shouldBe` Just (1 :: Int)
      decode (encode FlashSourceInternal) `shouldBe` Just (2 :: Int)

      parseFlashSource (0 :: Int) `shouldBe` Just FlashSourceNone
      parseFlashSource (1 :: Int) `shouldBe` Just FlashSourceExternal
      parseFlashSource (2 :: Int) `shouldBe` Just FlashSourceInternal

    it "checks that Maybe fields become null in JSON" $ do
      let province = mkSymbolizedItem ("CA"::Text)
          exif = def { exifCountry = Nothing, exifProvince = Just province }
          encoded = encode exif
          decoded = decode encoded :: Maybe Value
      case decoded of
        Just val -> do
            parseMaybe (withObject "test" (.: "country")) val `shouldBe` Just Null
            parseMaybe (withObject "test" (.: "province")) val `shouldBe` Just (String "CA")
        _ -> expectationFailure "Failed to decode as Value"

  describe "Orientation helpers" $ do
    it "maps orientations to transforms" $ do
      transformParams (affineTransform OrientationTopLeft)  `shouldBe` (0, False, False)
      transformParams (affineTransform OrientationTopRight) `shouldBe` (0, True, False)
      transformParams (affineTransform OrientationBotRight) `shouldBe` (0, True, True)
      transformParams (affineTransform OrientationBotLeft)  `shouldBe` (0, False, True)
      transformParams (affineTransform OrientationLeftTop)  `shouldBe` (-1, False, True)
      transformParams (affineTransform OrientationRightTop) `shouldBe` (1, False, False)
      transformParams (affineTransform OrientationRightBot) `shouldBe` (1, False, True)
      transformParams (affineTransform OrientationLeftBot)  `shouldBe` (-1, False, False)

    it "formats transform parameters and matrices" $ do
      let approx (a, b, c, d) (x, y, z, w) =
            and [ abs (a - x) < 1e-9
                , abs (b - y) < 1e-9
                , abs (c - z) < 1e-9
                , abs (d - w) < 1e-9
                ]
      transformParams (Transform RLeft True False) `shouldBe` (-1, True, False)
      transformMatrix (Transform RCenter False False) `shouldBe` (1, 0, 0, 1)
      transformMatrix (Transform RLeft False False) `shouldSatisfy` approx (0, -1, 1, 0)
      transformMatrix (Transform RRight False False) `shouldSatisfy` approx (0, 1, -1, 0)

  describe "Lens helpers" $ do
    let unknownNamedLens = LensInfo
          (mkSym "Unknown (Canon)")
          (mkSym "Canon EF 50mm")
          (Just (Prime 50))
          (Just (FixedAperture 1.8))
          (Just (mkSym "123"))
        zoomLens = LensInfo
          (mkSym "Canon EF 70-200mm")
          (mkSym "Canon EF 70-200mm")
          (Just (Zoom 70 200))
          (Just (VariableAperture 2.8 4.0))
          Nothing
        showLensType :: LensType -> String
        showLensType lt = case lt of
          LensPrime -> "prime"
          LensConstantApertureZoom -> "constant-zoom"
          LensVariableApertureZoom -> "variable-zoom"
          LensUnknown -> "unknown"
    it "chooses the most useful display name" $ do
      lensDisplayName unknownNamedLens `shouldBe` mkSym "Canon EF 50mm"
      lensDisplayName zoomLens `shouldBe` mkSym "Canon EF 70-200mm"

    it "formats short names and serials" $ do
      lensShortName unknownNamedLens `shouldBe` "Canon EF 50mm (#123)"
      lensShortName zoomLens `shouldBe` "Canon EF 70-200mm"

    it "classifies lens types" $ do
      showLensType (lensType (LensInfo (mkSym "prime") (mkSym "prime") (Just (Prime 50)) (Just (FixedAperture 1.8)) Nothing))
        `shouldBe` "prime"
      showLensType (lensType zoomLens) `shouldBe` "variable-zoom"
      case lensType (LensInfo (mkSym "fixed") (mkSym "fixed") (Just (Zoom 24 70)) (Just (FixedAperture 4.0)) Nothing) of
        LensConstantApertureZoom -> pure ()
        _ -> expectationFailure "Expected constant-zoom"
      showLensType (lensType (LensInfo (mkSym "unknown") (mkSym "unknown") Nothing Nothing Nothing))
        `shouldBe` "unknown"

  describe "fillCompositeExif" $ do
    let date d = Just (ExifTime (ZonedTime (LocalTime (fromGregorian 2024 6 d) (TimeOfDay 10 0 0)) (TimeZone 0 False "")))
        lens = LensInfo (mkSym "Lens A") (mkSym "Lens A") (Just (Prime 50)) Nothing Nothing
        component = def { exifCreateDate = date 1
                        , exifCamera = Just (mkSym "Cam A")
                        , exifModel = Just (mkSym "Model A")
                        , exifSerial = Just (mkSym "1")
                        , exifMake = Just (mkSym "Make A")
                        , exifLens = lens
                        , exifLensMake = Just (mkSym "Lens make A")
                        , exifCountry = Just (mkSym "Country A")
                        , exifCity = Just (mkSym "City A")
                        , exifPeople = setFromList [mkSym "Person A"]
                        , exifKeywords = setFromList [mkSym "Keyword A"]
                        , exifWidth = Just 6000
                        , exifHeight = Just 4000
                        , exifOrientation = OrientationRightTop
                        , exifISO = Just 100
                        }
    it "takes the date, camera, lens and place the composite lacks" $ do
      let filled = fillCompositeExif component def
      exifCreateDate filled `shouldBe` exifCreateDate component
      (exifCamera filled, exifModel filled, exifSerial filled, exifMake filled) `shouldBe`
        (exifCamera component, exifModel component, exifSerial component, exifMake component)
      (exifLens filled, exifLensMake filled) `shouldBe` (lens, exifLensMake component)
      (exifCountry filled, exifCity filled) `shouldBe` (exifCountry component, exifCity component)
    it "never takes the size, orientation, exposure, people or keywords" $ do
      let filled = fillCompositeExif component def
      (exifWidth filled, exifHeight filled) `shouldBe` (Nothing, Nothing)
      exifOrientation filled `shouldBe` OrientationTopLeft
      exifISO filled `shouldBe` Nothing
      exifPeople filled `shouldBe` mempty
      exifKeywords filled `shouldBe` mempty
    it "keeps what the composite has, taking each group whole or not at all" $ do
      let composite = def { exifCreateDate = date 5
                          , exifModel = Just (mkSym "Model B")
                          , exifLens = unknownLens { liFL = Just (Prime 24) }
                          , exifLocation = Just (mkSym "Place B")
                          }
          filled = fillCompositeExif component composite
      filled `shouldBe` composite
    it "fills each group on its own" $ do
      let composite = def { exifCreateDate = date 5
                          , exifCamera = Just (mkSym "Cam B")
                          }
          filled = fillCompositeExif component composite
      exifCreateDate filled `shouldBe` date 5
      -- The camera is the composite's own, without the component's
      -- serial number or make.
      (exifCamera filled, exifModel filled, exifSerial filled, exifMake filled) `shouldBe`
        (Just (mkSym "Cam B"), Nothing, Nothing, Nothing)
      -- The lens and the place come from the component.
      (exifLens filled, exifLensMake filled) `shouldBe` (lens, exifLensMake component)
      (exifCountry filled, exifCity filled) `shouldBe` (exifCountry component, exifCity component)
    it "keeps a group whole when the composite has any one of its fields" $ do
      let sym = Just (mkSym "B")
          camera e = (exifCamera e, exifModel e, exifSerial e, exifMake e)
          place e = (exifCountry e, exifProvince e, exifCity e, exifLocation e)
          lensOf e = (exifLens e, exifLensMake e)
          sameCamera a b = camera a == camera b
          samePlace a b = place a == place b
          sameLens a b = lensOf a == lensOf b
          -- A composite with one field of a group, and that group.
          composites =
            [ (def { exifCamera = sym }, sameCamera), (def { exifModel = sym }, sameCamera)
            , (def { exifSerial = sym }, sameCamera), (def { exifMake = sym }, sameCamera)
            , (def { exifCountry = sym }, samePlace), (def { exifProvince = sym }, samePlace)
            , (def { exifCity = sym }, samePlace), (def { exifLocation = sym }, samePlace)
            , (def { exifLensMake = sym }, sameLens)
            , (def { exifLens = unknownLens { liSerial = sym } }, sameLens)
            ]
      forM_ composites $ \(composite, sameGroup) ->
        fillCompositeExif component composite `shouldSatisfy` sameGroup composite

  describe "Person formatting" $ do
    it "formats slash and space-separated names" $ do
      formatPerson False "Doe/John" `shouldBe` "John Doe"
      formatPerson True "Doe/John" `shouldBe` "John D."
      formatPerson False "John Doe" `shouldBe` "John Doe"
      formatPerson False "SingleName" `shouldBe` "SingleName"

  describe "NFData" $ do
    it "forces previously skipped exif fields" $ do
      evaluate (rnf (def :: Exif)) `shouldReturn` ()
      evaluate (rnf (def { exifMake = Just (error "make") } :: Exif))
        `shouldThrow` anyErrorCall
      evaluate (rnf (def { exifLensMake = Just (error "lensmake") } :: Exif))
        `shouldThrow` anyErrorCall

  describe "FlashSource parseFlashSource function" $ do
    it "handles valid values" $ do
      parseFlashSource (0 :: Int) `shouldBe` Just FlashSourceNone
      parseFlashSource (1 :: Int) `shouldBe` Just FlashSourceExternal
      parseFlashSource (2 :: Int) `shouldBe` Just FlashSourceInternal

    it "handles invalid values" $ do
      parseFlashSource (3 :: Int) `shouldBe` Nothing
      parseFlashSource (-1 :: Int) `shouldBe` Nothing
      parseFlashSource (999 :: Int) `shouldBe` Nothing
