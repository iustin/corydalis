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
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module StatsSpec (spec) where

import           Data.Default
import           Data.Fixed          ()
import qualified Data.Map.Strict     as Map
import           Data.Semigroup
import qualified Data.Text
import           Data.Time.LocalTime
import           System.Posix.Types  (FileOffset)

import           Data.Text.Arbitrary ()

import           Stats
import           TestImport

-- Helper (orphan) instances

instance Default Text where
  def = Data.Text.empty

-- Helper functions for creating test data

mkLocalTime :: Integer -> Int -> Int -> Int -> Int -> Int -> LocalTime
mkLocalTime year month day hour minute second' =
  LocalTime (fromGregorian year month day) (TimeOfDay hour minute (fromIntegral second'))

mkDateRange :: LocalTime -> LocalTime -> DateRange
mkDateRange = (,)

sampleDateRange1 :: DateRange
sampleDateRange1 = mkDateRange (mkLocalTime 2023 1 1 10 0 0) (mkLocalTime 2023 1 1 15 0 0)

sampleDateRange2 :: DateRange
sampleDateRange2 = mkDateRange (mkLocalTime 2023 1 2 8 0 0) (mkLocalTime 2023 1 2 18 0 0)

overlappingDateRange :: DateRange
overlappingDateRange = mkDateRange (mkLocalTime 2023 1 1 12 0 0) (mkLocalTime 2023 1 3 12 0 0)

sampleTrends :: Trends
sampleTrends = Map.fromList [((2023, 1), 5), ((2023, 2), 3)]

sampleCameraInfo1 :: CameraInfo
sampleCameraInfo1 = CameraInfo "Canon EOS R5" (Just (1000, 2000))

sampleCameraInfo2 :: CameraInfo
sampleCameraInfo2 = CameraInfo "Nikon D850" (Just (500, 1500))

sampleCameraInfo3 :: CameraInfo
sampleCameraInfo3 = CameraInfo "Sony A7R IV" Nothing

spec :: Spec
spec = parallel $ do
  describe "mergeMinMaxPair" $ do
    it "merges Nothing values correctly" $ do
      -- explicit typing information
      let nothing = Nothing::Maybe (Int, Int)
      mergeMinMaxPair Nothing Nothing `shouldBe` nothing
      mergeMinMaxPair (Just (1, 5)) nothing `shouldBe` Just (1, 5)
      mergeMinMaxPair nothing (Just (2, 6)) `shouldBe` Just (2, 6)

    it "merges overlapping ranges correctly" $ do
      let range1 = Just (1, 10)::Maybe (Int, Int)
          range2 = Just (5, 15)
          expected = Just (1, 15)
      mergeMinMaxPair range1 range2 `shouldBe` expected

    it "merges non-overlapping ranges correctly" $ do
      let range1 = Just (1, 5)::Maybe (Int, Int)
          range2 = Just (10, 15)
          expected = Just (1, 15)
      mergeMinMaxPair range1 range2 `shouldBe` expected

    it "handles identical ranges" $ do
      let range1 = Just (5, 10)::Maybe (Int, Int)
          range2 = Just (5, 10)
          expected = Just (5, 10)
      mergeMinMaxPair range1 range2 `shouldBe` expected

  describe "Occurrence" $ do
    describe "Default instance" $ do
      it "creates empty occurrence with default values" $ do
        let empty' = def :: Occurrence Text
        ocFiles empty' `shouldBe` 0
        ocFileSize empty' `shouldBe` 0
        ocFolders empty' `shouldBe` 0
        ocData empty' `shouldBe` ""
        ocTrends empty' `shouldBe` Map.empty
        ocDateRange empty' `shouldBe` Nothing

    describe "Semigroup instance" $ do
      it "combines two occurrences correctly" $ do
        let occ1 = Occurrence 2 1000 1 ("data1" :: Text) sampleTrends (Just sampleDateRange1)
            occ2 = Occurrence 3 2000 2 ("data2" :: Text) (Map.singleton (2023, 3) 2) (Just sampleDateRange2)
            combined = occ1 <> occ2

        ocFiles combined `shouldBe` 5
        ocFileSize combined `shouldBe` 3000
        ocFolders combined `shouldBe` 3
        ocData combined `shouldBe` "data1data2"
        Map.lookup (2023, 1) (ocTrends combined) `shouldBe` Just 5
        Map.lookup (2023, 2) (ocTrends combined) `shouldBe` Just 3
        Map.lookup (2023, 3) (ocTrends combined) `shouldBe` Just 2

      it "merges date ranges correctly when combining" $ do
        let occ1 = Occurrence 1 100 0 ("a" :: Text) Map.empty (Just sampleDateRange1)
            occ2 = Occurrence 1 200 0 ("b" :: Text) Map.empty (Just overlappingDateRange)
            combined = occ1 <> occ2

        case ocDateRange combined of
          Just (start, end) -> do
            start `shouldBe` mkLocalTime 2023 1 1 10 0 0
            end `shouldBe` mkLocalTime 2023 1 3 12 0 0
          Nothing -> expectationFailure "Expected merged date range"

      it "handles empty occurrences in combination" $ do
        let empty' = def :: Occurrence Text
            occ = Occurrence 1 500 0 "test" Map.empty Nothing
            combined1 = empty' <> occ
            combined2 = occ <> empty'

        combined1 `shouldBe` occ
        combined2 `shouldBe` occ

      it "combines trends by summing values for same keys" $ do
        let trends1 = Map.fromList [((2023, 1), 10), ((2023, 2), 5)]
            trends2 = Map.fromList [((2023, 1), 3), ((2023, 3), 7)]
            occ1 = Occurrence 1 100 0 ("a" :: Text) trends1 Nothing
            occ2 = Occurrence 1 200 0 ("b" :: Text) trends2 Nothing
            combined = occ1 <> occ2

        Map.lookup (2023, 1) (ocTrends combined) `shouldBe` Just 13  -- 10 + 3
        Map.lookup (2023, 2) (ocTrends combined) `shouldBe` Just 5   -- only in trends1
        Map.lookup (2023, 3) (ocTrends combined) `shouldBe` Just 7   -- only in trends2

    describe "Monoid instance" $ do
      it "has correct mempty" $ do
        let empty' = mempty :: Occurrence Text
        empty' `shouldBe` def

      it "satisfies left identity" $ do
        let occ = Occurrence 1 500 0 ("test" :: Text) sampleTrends (Just sampleDateRange1)
        mempty <> occ `shouldBe` occ

      it "satisfies right identity" $ do
        let occ = Occurrence 1 500 0 ("test" :: Text) sampleTrends (Just sampleDateRange1)
        occ <> mempty `shouldBe` occ

  describe "ocFromSize" $ do
    it "creates occurrence from size and data" $ do
      let size = 1024 :: FileOffset
          testData = "test data" :: Text
          trendsKey = Just (2023, 5)
          dateRange = Just sampleDateRange1
          occ = ocFromSize size testData trendsKey dateRange

      ocFiles occ `shouldBe` 1
      ocFileSize occ `shouldBe` 1024
      ocFolders occ `shouldBe` 0
      ocData occ `shouldBe` "test data"
      ocDateRange occ `shouldBe` Just sampleDateRange1

    it "creates occurrence with empty trends when no trends key" $ do
      let occ = ocFromSize 100 ("test" :: Text) Nothing Nothing
      ocTrends occ `shouldBe` Map.empty

    it "creates occurrence with single trend when trends key provided" $ do
      let occ = ocFromSize 100 ("test" :: Text) (Just (2023, 4)) Nothing
      ocTrends occ `shouldBe` Map.singleton (2023, 4) 1

  describe "CameraInfo" $ do
    describe "Default instance" $ do
      it "creates default camera info" $ do
        let defaultCam = def :: CameraInfo
        ciName defaultCam `shouldBe` "unknown"
        ciShutterCount defaultCam `shouldBe` Nothing

    describe "Semigroup instance" $ do
      it "combines camera info keeping first name and merging shutter counts" $ do
        let combined = sampleCameraInfo1 <> sampleCameraInfo2
        ciName combined `shouldBe` "Canon EOS R5"
        ciShutterCount combined `shouldBe` Just (500, 2000)  -- min of mins, max of maxes

      it "handles camera with no shutter count" $ do
        let combined1 = sampleCameraInfo1 <> sampleCameraInfo3
            combined2 = sampleCameraInfo3 <> sampleCameraInfo1
        ciName combined1 `shouldBe` "Canon EOS R5"
        ciShutterCount combined1 `shouldBe` Just (1000, 2000)
        ciName combined2 `shouldBe` "Sony A7R IV"
        ciShutterCount combined2 `shouldBe` Just (1000, 2000)

      it "handles both cameras without shutter count" $ do
        let cam1 = CameraInfo "Camera 1" Nothing
            cam2 = CameraInfo "Camera 2" Nothing
            combined = cam1 <> cam2
        ciName combined `shouldBe` "Camera 1"
        ciShutterCount combined `shouldBe` Nothing

    describe "Ord instance" $ do
      it "orders camera info correctly" $ do
        sampleCameraInfo1 `shouldSatisfy` (< sampleCameraInfo2)  -- "Canon" < "Nikon"
        sampleCameraInfo2 `shouldSatisfy` (< sampleCameraInfo3)  -- "Nikon" < "Sony"

    describe "Eq instance" $ do
      it "compares camera info for equality" $ do
        let cam1 = CameraInfo "Test" (Just (100, 200))
            cam2 = CameraInfo "Test" (Just (100, 200))
            cam3 = CameraInfo "Test" (Just (150, 200))
        cam1 `shouldBe` cam2
        cam1 `shouldNotBe` cam3

  describe "Type aliases and helper types" $ do
    it "creates DateRange correctly" $ do
      let start = mkLocalTime 2023 1 1 10 0 0
          end = mkLocalTime 2023 1 1 15 0 0
          range = (start, end) :: DateRange
      fst range `shouldBe` start
      snd range `shouldBe` end

    it "creates DayRange correctly" $ do
      let startDay = fromGregorian 2023 1 1
          endDay = fromGregorian 2023 1 31
          range = (startDay, endDay) :: DayRange
      fst range `shouldBe` startDay
      snd range `shouldBe` endDay

    it "creates Trends map correctly" $ do
      let trends = Map.fromList [((2023, 1), 10), ((2023, 2), 15)] :: Trends
      Map.size trends `shouldBe` 2
      Map.lookup (2023, 1) trends `shouldBe` Just 10
      Map.lookup (2023, 2) trends `shouldBe` Just 15
      Map.lookup (2023, 3) trends `shouldBe` Nothing

  describe "Property-based tests" $ do
    prop "Occurrence semigroup associativity" $ \a b c ->
      let occ1 = Occurrence a (fromIntegral a) (fromIntegral a) (show a) Map.empty Nothing
          occ2 = Occurrence b (fromIntegral b) (fromIntegral b) (show b) Map.empty Nothing
          occ3 = Occurrence c (fromIntegral c) (fromIntegral c) (show c) Map.empty Nothing
      in (occ1 <> occ2) <> occ3 `shouldBe` occ1 <> (occ2 <> occ3)

    prop "CameraInfo semigroup preserves first name" $ \name1 name2 ->
      let cam1 = CameraInfo name1 Nothing
          cam2 = CameraInfo name2 Nothing
          combined = cam1 <> cam2
      in ciName combined `shouldBe` name1

    prop "mergeMinMaxPair is commutative" $ \a b c d ->
      let range1 = Just (min a b, max a b)::Maybe (Int, Int)
          range2 = Just (min c d, max c d)
      in mergeMinMaxPair range1 range2 `shouldBe` mergeMinMaxPair range2 range1

    prop "mergeMinMaxPair is associative" $ \a b c d e f ->
      let range1 = Just (min a b, max a b)::Maybe (Int, Int)
          range2 = Just (min c d, max c d)
          range3 = Just (min e f, max e f)
          result1 = mergeMinMaxPair (mergeMinMaxPair range1 range2) range3
          result2 = mergeMinMaxPair range1 (mergeMinMaxPair range2 range3)
      in result1 `shouldBe` result2
