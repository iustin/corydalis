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
      let exif = def { exifCountry = Nothing, exifProvince = Just "CA" }
          encoded = encode exif
          decoded = decode encoded :: Maybe Value
      case decoded of
        Just val -> do
            parseMaybe (withObject "test" (.: "country")) val `shouldBe` Just Null
            parseMaybe (withObject "test" (.: "province")) val `shouldBe` Just (String "CA")
        _ -> expectationFailure "Failed to decode as Value"

  describe "FlashSource parseFlashSource function" $ do
    it "handles valid values" $ do
      parseFlashSource (0 :: Int) `shouldBe` Just FlashSourceNone
      parseFlashSource (1 :: Int) `shouldBe` Just FlashSourceExternal
      parseFlashSource (2 :: Int) `shouldBe` Just FlashSourceInternal

    it "handles invalid values" $ do
      parseFlashSource (3 :: Int) `shouldBe` Nothing
      parseFlashSource (-1 :: Int) `shouldBe` Nothing
      parseFlashSource (999 :: Int) `shouldBe` Nothing
