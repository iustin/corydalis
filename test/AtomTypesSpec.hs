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

module AtomTypesSpec (spec) where

import           Test.QuickCheck (chooseEnum, forAll, suchThat)

import           AtomTypes
import           TestImport

spec :: Spec
spec = parallel $ do
  describe "show helpers" $ do
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

  describe "parseSeason" $ do
    it "parses season names" $ do
      parseSeason "winter" `shouldBe` Just Winter
      parseSeason "Summer" `shouldBe` Just Summer
      parseSeason "nope" `shouldBe` Nothing

  describe "parseMonth" $ do
    it "parses month names and numbers" $ do
      parseMonth "january" `shouldBe` Just January
      parseMonth "12" `shouldBe` Just December
      parseMonth "13" `shouldBe` Nothing

  describe "parseDay" $ do
    it "parses weekdays, weekends and ordinals" $ do
      parseDay "monday" `shouldBe` Just Monday
      parseDay "weekend" `shouldBe` Just Weekend
      parseDay "1st" `shouldBe` Just (MonthDay 1)
      parseDay "31" `shouldBe` Just (MonthDay 31)

  describe "parseEventKind" $ do
    it "parses event kinds" $ do
      parseEventKind "birthday" `shouldBe` Just EKBirthday
      parseEventKind "noevent" `shouldBe` Just EKNoEvent
      parseEventKind "nope" `shouldBe` Nothing

  describe "extractEventType" $ do
    it "classifies events" $ do
      extractEventType (Just BirthdayEvent { eventName = "x", eventPeople = [] })
        `shouldBe` EKBirthday
      extractEventType Nothing `shouldBe` EKNoEvent

  describe "flash parsing roundtrip" $ do
    prop "any valid flash value is parsed correctly" $
      forAll (chooseEnum (minBound, maxBound) `suchThat` (/= FlashUnknown)) $ \flash ->
        parseFlash (showFlash flash) `shouldBe` Just flash
