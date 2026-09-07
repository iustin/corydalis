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
import           Data.Aeson.Types    (parseMaybe)
import           Data.Text.Arbitrary ()
import           Test.QuickCheck

-- Arbitrary instances for testing

instance Arbitrary Orientation where
  arbitrary = chooseEnum (minBound, maxBound)

instance Arbitrary FlashSource where
  arbitrary = chooseEnum (minBound, maxBound)

spec :: Spec
spec = parallel $ do
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

  describe "Person formatting" $ do
    it "formats slash and space-separated names" $ do
      formatPerson False "Doe/John" `shouldBe` "John Doe"
      formatPerson True "Doe/John" `shouldBe` "John D."
      formatPerson False "John Doe" `shouldBe` "John Doe"
      formatPerson False "SingleName" `shouldBe` "SingleName"

  describe "FlashSource parseFlashSource function" $ do
    it "handles valid values" $ do
      parseFlashSource (0 :: Int) `shouldBe` Just FlashSourceNone
      parseFlashSource (1 :: Int) `shouldBe` Just FlashSourceExternal
      parseFlashSource (2 :: Int) `shouldBe` Just FlashSourceInternal

    it "handles invalid values" $ do
      parseFlashSource (3 :: Int) `shouldBe` Nothing
      parseFlashSource (-1 :: Int) `shouldBe` Nothing
      parseFlashSource (999 :: Int) `shouldBe` Nothing
