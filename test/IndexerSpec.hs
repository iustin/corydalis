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

module IndexerSpec (spec) where

import           Data.Default
import qualified Data.Text           as Text
import           Data.Time.LocalTime

import           Yesod.Core          (fromPathPiece, toPathPiece)

import           GHC.Stack           ()
import           Test.QuickCheck     (Arbitrary (..), chooseEnum, forAll,
                                      suchThat)

import           Exif
import           Indexer
import           Pics
import           TestImport
import           Types

instance Arbitrary FolderClass where
  arbitrary = chooseEnum (minBound, maxBound)

atomDescContains :: HasCallStack => Atom -> Text -> Expectation
atomDescContains atom str =
  str `shouldSatisfy` (`Text.isInfixOf` atomDescription atom)

spec :: Spec
spec = parallel $ do
  describe "Symbol" $ do
    it "converts to PathPiece and back" $ do
      let symbols = [minBound..maxBound] :: [Symbol]
      let pathPieces = map toPathPiece symbols
      map fromPathPiece pathPieces `shouldBe` map Just symbols

    it "symbolName returns the correct names" $ do
      symbolName TCountry `shouldBe` "country"
      symbolName TPerson `shouldBe` "person"
      symbolName TYear `shouldBe` "year"

    it "negSymbolName adds no- prefix" $ do
      negSymbolName TCountry `shouldBe` "no-country"
      negSymbolName TKeyword `shouldBe` "no-keyword"

  describe "parseAtom" $ do
    it "parses country atoms" $ do
      parseAtom "country" "Spain" `shouldBe` Just (Country (OpEqual "Spain"))
      parseAtom "country" "~Spain" `shouldBe` Just (Country (OpFuzzy (makeFuzzy "Spain")))
      parseAtom "no-country" "" `shouldBe` Just (Country OpMissing)

    it "parses numeric atoms" $ do
      parseAtom "year" "2020" `shouldBe` Just (Year (OpEq 2020))
      parseAtom "year" "<2020" `shouldBe` Just (Year (OpLt 2020))
      parseAtom "year" ">=2020" `shouldBe` Just (Year (OpGe 2020))

    it "parses f-stop atoms" $ do
      parseAtom "f-stop" "2.8" `shouldBe` Just (FStop (OpEq 2.8))
      parseAtom "f-stop" ">4" `shouldBe` Just (FStop (OpGt 4.0))

    it "parses season atoms" $ do
      parseAtom "season" "winter" `shouldBe` Just (Season Winter)
      parseAtom "season" "summer" `shouldBe` Just (Season Summer)

    it "parses month atoms" $ do
      parseAtom "month" "january" `shouldBe` Just (Month January)
      parseAtom "month" "12" `shouldBe` Just (Month December)

    it "parses day atoms" $ do
      parseAtom "day" "monday" `shouldBe` Just (Day Monday)
      parseAtom "day" "weekend" `shouldBe` Just (Day Weekend)
      parseAtom "day" "1st" `shouldBe` Just (Day (MonthDay 1))
      parseAtom "day" "31" `shouldBe` Just (Day (MonthDay 31))

    it "parses flash source atoms" $ do
      parseAtom "flash-source" "none" `shouldBe` Just (FlashSrc FlashNone)
      parseAtom "flash-source" "internal" `shouldBe` Just (FlashSrc FlashInternal)
      parseAtom "flash-source" "ext" `shouldBe` Just (FlashSrc FlashExternal)

    it "parses megapixels atoms" $ do
      parseAtom "megapixels" "24.2" `shouldBe` Just (Megapixels (OpEq 24.2))
      parseAtom "megapixels" ">20" `shouldBe` Just (Megapixels (OpGt 20.0))

  describe "atomToParams" $ do
    it "converts simple atoms back to params" $ do
      atomToParams (Country (OpEqual "Spain")) `shouldBe` [("country", "Spain")]
      atomToParams (Year (OpEq 2020)) `shouldBe` [("year", "2020")]
      atomToParams (FStop (OpGt 2.8)) `shouldBe` [("f-stop", ">2.8")]

    it "converts combined atoms correctly" $ do
      let atom = And (Country (OpEqual "Spain")) (Year (OpEq 2020))
      atomToParams atom `shouldBe` [("country", "Spain"), ("year", "2020"), ("and", "")]

      let atom2 = Or (Month January) (Month February)
      atomToParams atom2 `shouldBe` [("month", "January"), ("month", "February"), ("or", "")]

  describe "atomDescription" $ do
    it "describes country atoms" $ do
      atomDescContains (Country (OpEqual "Spain")) "country is Spain"
      atomDescContains (Country OpMissing) "has no country information"

    it "describes person atoms" $ do
      atomDescContains (Person (OpEqual "John")) "John is in the picture"
      atomDescContains (Person (OpFuzzy (makeFuzzy "John"))) "tagged with a person named like john"

    it "describes year atoms" $ do
      atomDescContains (Year (OpEq 2020)) "taken in the year 2020"
      atomDescContains (Year (OpLt 2020)) "taken before the year 2020"

    it "describes combined atoms" $ do
      let atom = And (Country (OpEqual "Spain")) (Year (OpEq 2020))
      atomDescContains atom "country is Spain"
      atomDescContains atom  "taken in the year 2020"

  describe "atomFindsFiles" $ do
    it "returns True for most atoms" $ do
      atomFindsFiles (Country (OpEqual "Spain")) `shouldBe` True
      atomFindsFiles (Year (OpEq 2020)) `shouldBe` True

    prop "returns False for folder class atoms" $ \fclass ->
      atomFindsFiles (FClass fclass) `shouldBe` False

    it "properly handles combined atoms" $ do
      atomFindsFiles (And (Country (OpEqual "Spain")) (Year (OpEq 2020))) `shouldBe` True
      atomFindsFiles (And (Country (OpEqual "Spain")) (FClass FolderStandalone)) `shouldBe` False
      atomFindsFiles (Or (FClass FolderMixed) (Year (OpEq 2020))) `shouldBe` True

  describe "parseAtomParams" $ do
    it "parses simple atom params" $ do
      parseAtomParams [("country", "Spain")] `shouldBe` Right (Country (OpEqual "Spain"))
      parseAtomParams [("year", "2020")] `shouldBe` Right (Year (OpEq 2020))

    it "parses multiple atom params" $ do
      let params = [("country", "Spain"), ("year", "2020"), ("and", "")]
      case parseAtomParams params of
        Right (And a b) -> do
          [a,b] `shouldMatchList` [Country (OpEqual "Spain"), Year (OpEq 2020)]
        _ -> expectationFailure "Failed to parse atom params correctly"

    it "returns an error for too many params" $ do
      let tooManyParams = replicate 51 ("country", "Spain")
      case parseAtomParams tooManyParams of
        Left err -> err `shouldSatisfy` ("Too many search parameters" `Text.isInfixOf`)
        Right _ -> expectationFailure "Should have returned an error for too many params"

  describe "picDay, picMonth and picSeason" $ do
    let testDate = LocalTime
                    { localDay = fromGregorian 2020 7 15
                    , localTimeOfDay = TimeOfDay 12 0 0
                    }
        testExifTime = ExifTime $ ZonedTime testDate utc
    let testExif = def { exifCreateDate = Just testExifTime }
    let testImage = Image
                      { imgName = ImageName "test.jpg"
                      , imgParent = "test"
                      , imgStatus = ImageStandalone
                      , imgType = MediaImage
                      , imgExif = testExif
                      , imgRawPath = Nothing
                      , imgSidecarPath = Nothing
                      , imgJpegPath = [simpleFile "test.jpg"]
                      , imgMasterMov = Nothing
                      , imgMovs = []
                      , imgUntracked = []
                      , imgRange = Nothing
                      , imgFlags = def
                      }

    it "extracts day correctly" $ do
      picDay testImage `shouldBe` Just Wednesday
      picMonthDay testImage `shouldBe` Just (MonthDay 15)

    it "extracts month correctly" $ do
      picMonth testImage `shouldBe` Just July

    it "extracts season correctly" $ do
      picSeason testImage `shouldBe` Just Summer

  describe "ShowText instances" $ do
    it "shows season correctly" $ do
      showSeason Winter `shouldBe` "winter"
      showSeason Summer `shouldBe` "summer"

    it "shows month correctly" $ do
      showMonth January `shouldBe` "January"
      showMonth December `shouldBe` "December"

    it "shows day correctly" $ do
      showDay Monday `shouldBe` "Monday"
      showDay Weekday `shouldBe` "weekday"
      showDay (MonthDay 1) `shouldBe` "1st"
      showDay (MonthDay 2) `shouldBe` "2nd"
      showDay (MonthDay 3) `shouldBe` "3rd"
      showDay (MonthDay 4) `shouldBe` "4th"

    it "shows media type correctly" $ do
      showMedia MediaImage `shouldBe` "image"
      showMedia MediaMovie `shouldBe` "movie"

  describe "parseString, parseNumDecimal and parseNumReal" $ do
    it "parses fuzzy strings" $ do
      parseString "~test" `shouldBe` Just (OpFuzzy (makeFuzzy "test"))

    it "parses normal strings" $ do
      parseString "test" `shouldBe` Just (OpEqual "test")

    it "parses decimal numbers" $ do
      parseNumDecimal "123" `shouldBe` Just (OpEq 123 :: NumOp Int)
      parseNumDecimal "<123" `shouldBe` Just (OpLt 123 :: NumOp Int)
      parseNumDecimal ">=123" `shouldBe` Just (OpGe 123 :: NumOp Int)

    it "parses real numbers" $ do
      parseNumReal "2.8" `shouldBe` Just (OpEq 2.8)
      parseNumReal "<2.8" `shouldBe` Just (OpLt 2.8)
      parseNumReal ">=2.8" `shouldBe` Just (OpGe 2.8)

  describe "parseShutterSpeed" $ do
    it "parses fraction format" $ do
      parseShutterSpeed "1/250s" `shouldBe` Just (OpEq 0.004)
      parseShutterSpeed "1/1000" `shouldBe` Just (OpEq 0.001)

    it "parses decimal format" $ do
      parseShutterSpeed "0.5s" `shouldBe` Just (OpEq 0.5)
      parseShutterSpeed "2s" `shouldBe` Just (OpEq 2.0)

    it "parses with operators" $ do
      parseShutterSpeed ">1/250s" `shouldBe` Just (OpGt 0.004)
      parseShutterSpeed "<=2s" `shouldBe` Just (OpLe 2.0)

  describe "showShutterSpeed" $ do
    it "formats speeds less than 1 second as fractions" $ do
      showShutterSpeed 0.004 `shouldBe` "1/250s"
      showShutterSpeed 0.001 `shouldBe` "1/1000s"

    it "formats speeds 1 second or longer as decimals" $ do
      showShutterSpeed 1.0 `shouldBe` "1s"
      showShutterSpeed 2.5 `shouldBe` "2.5s"

  describe "weekdayToEnd" $ do
    it "classifies weekends correctly" $ do
      weekdayToEnd Saturday `shouldBe` Weekend
      weekdayToEnd Sunday `shouldBe` Weekend

    it "classifies weekdays correctly" $ do
      weekdayToEnd Monday `shouldBe` Weekday
      weekdayToEnd Friday `shouldBe` Weekday

  describe "monthToSeason" $ do
    it "maps winter months correctly" $ do
      monthToSeason December `shouldBe` Just Winter
      monthToSeason January `shouldBe` Just Winter
      monthToSeason February `shouldBe` Just Winter

    it "maps spring months correctly" $ do
      monthToSeason March `shouldBe` Just Spring
      monthToSeason April `shouldBe` Just Spring
      monthToSeason May `shouldBe` Just Spring

    it "maps summer months correctly" $ do
      monthToSeason June `shouldBe` Just Summer
      monthToSeason July `shouldBe` Just Summer
      monthToSeason August `shouldBe` Just Summer

    it "maps autumn months correctly" $ do
      monthToSeason September `shouldBe` Just Autumn
      monthToSeason October `shouldBe` Just Autumn
      monthToSeason November `shouldBe` Just Autumn

  describe "intToMonth and intToWeekDay" $ do
    it "converts integers to months" $ do
      intToMonth 1 `shouldBe` Just January
      intToMonth 12 `shouldBe` Just December
      intToMonth 13 `shouldBe` Nothing

    it "converts integers to weekdays" $ do
      intToWeekDay 1 `shouldBe` Just Monday
      intToWeekDay 7 `shouldBe` Just Sunday
      intToWeekDay 8 `shouldBe` Nothing

  describe "flash parsing roundtrip" $ do
    prop "any valid flash value is parsed correctly" $
      forAll (chooseEnum (minBound, maxBound) `suchThat` (/= FlashUnknown)) $ \flash ->
        parseFlash (showFlash flash) `shouldBe` Just flash
