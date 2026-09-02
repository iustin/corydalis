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

import           Test.QuickCheck (choose, chooseEnum, forAll, suchThat)

import           AtomTypes
import           TestImport

spec :: Spec
spec = parallel $ do
  describe "show helpers" $ do
    it "shows season correctly" $ do
      showSeason Winter `shouldBe` "winter"
      showSeason Summer `shouldBe` "summer"
      showSeason SeasonUnknown `shouldBe` "unknown"

    it "shows month correctly" $ do
      showMonth January `shouldBe` "January"
      showMonth December `shouldBe` "December"
      showMonth MonthUnknown `shouldBe` "unknown"

    it "shows day correctly" $ do
      showDay Monday `shouldBe` "Monday"
      showDay Weekday `shouldBe` "weekday"
      showDay (MonthDay 1) `shouldBe` "1st"
      showDay (MonthDay 2) `shouldBe` "2nd"
      showDay (MonthDay 3) `shouldBe` "3rd"
      showDay (MonthDay 4) `shouldBe` "4th"
      showDay DayUnknown `shouldBe` "unknown"

    it "shows flash correctly" $ do
      showFlash FlashNone `shouldBe` "none"
      showFlash FlashUnknown `shouldBe` "unknown"

    it "shows event kind correctly" $ do
      showEventKind EKBirthday `shouldBe` "birthday"
      showEventKind EKNoEvent `shouldBe` "noevent"

  describe "showOrdinal" $ do
    it "uses English ordinal suffixes" $ do
      showOrdinal (1 :: Int) `shouldBe` "1st"
      showOrdinal (2 :: Int) `shouldBe` "2nd"
      showOrdinal (3 :: Int) `shouldBe` "3rd"
      showOrdinal (4 :: Int) `shouldBe` "4th"
      showOrdinal (11 :: Int) `shouldBe` "11th"
      showOrdinal (12 :: Int) `shouldBe` "12th"
      showOrdinal (13 :: Int) `shouldBe` "13th"
      showOrdinal (21 :: Int) `shouldBe` "21st"
      showOrdinal (31 :: Int) `shouldBe` "31st"

    it "does not suffix negative numbers" $
      showOrdinal (-1 :: Int) `shouldBe` "-1"

  describe "weekdayToEnd" $ do
    it "classifies weekends correctly" $ do
      weekdayToEnd Saturday `shouldBe` Weekend
      weekdayToEnd Sunday `shouldBe` Weekend

    it "classifies weekdays correctly" $ do
      weekdayToEnd Monday `shouldBe` Weekday
      weekdayToEnd Friday `shouldBe` Weekday

    it "classifies non-weekday values as weekday" $ do
      weekdayToEnd Weekend `shouldBe` Weekday
      weekdayToEnd (MonthDay 15) `shouldBe` Weekday

    prop "classifies converted weekdays" $
      forAll (choose (1, 7)) $ \n ->
        case intToWeekDay n of
          Just day -> weekdayToEnd day `shouldBe`
            if n >= 6 then Weekend else Weekday
          Nothing -> expectationFailure "intToWeekDay rejected a valid day"

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

    it "maps unknown month correctly" $ do
      monthToSeason MonthUnknown `shouldBe` Nothing

    prop "maps every known month" $
      forAll (chooseEnum (minBound, maxBound) `suchThat` (/= MonthUnknown)) $ \month ->
        isJust (monthToSeason month)

  describe "intToMonth and intToWeekDay" $ do
    it "converts integers to months" $ do
      intToMonth 1 `shouldBe` Just January
      intToMonth 12 `shouldBe` Just December
      intToMonth 13 `shouldBe` Nothing

    it "converts integers to weekdays" $ do
      intToWeekDay 1 `shouldBe` Just Monday
      intToWeekDay 7 `shouldBe` Just Sunday
      intToWeekDay 8 `shouldBe` Nothing

    prop "maps 1-12 to months" $
      forAll (choose (1, 12)) $ \n ->
        isJust (intToMonth n)

    prop "rejects months outside 1-12" $
      forAll (choose (-20, 40) `suchThat` (\n -> n < 1 || n > 12)) $ \n ->
        intToMonth n `shouldBe` Nothing

    prop "maps 1-7 to weekdays" $
      forAll (choose (1, 7)) $ \n ->
        isJust (intToWeekDay n)

    prop "rejects weekdays outside 1-7" $
      forAll (choose (-20, 40) `suchThat` (\n -> n < 1 || n > 7)) $ \n ->
        intToWeekDay n `shouldBe` Nothing

  describe "parseSeason" $ do
    it "parses season names" $ do
      parseSeason "winter" `shouldBe` Just Winter
      parseSeason "Summer" `shouldBe` Just Summer
      parseSeason "nope" `shouldBe` Nothing
      parseSeason "unknown" `shouldBe` Nothing
      parseSeason "fall" `shouldBe` Nothing

    prop "roundtrips known seasons" $
      forAll (chooseEnum (minBound, maxBound) `suchThat` (/= SeasonUnknown)) $ \season ->
        parseSeason (showSeason season) `shouldBe` Just season

  describe "parseMonth" $ do
    it "parses month names and numbers" $ do
      parseMonth "january" `shouldBe` Just January
      parseMonth "12" `shouldBe` Just December
      parseMonth "13" `shouldBe` Nothing
      parseMonth "unknown" `shouldBe` Nothing

    prop "roundtrips known months" $
      forAll (chooseEnum (minBound, maxBound) `suchThat` (/= MonthUnknown)) $ \month ->
        parseMonth (showMonth month) `shouldBe` Just month

  describe "parseDay" $ do
    it "parses weekdays, weekends and ordinals" $ do
      parseDay "monday" `shouldBe` Just Monday
      parseDay "weekend" `shouldBe` Just Weekend
      parseDay "1st" `shouldBe` Just (MonthDay 1)
      parseDay "11th" `shouldBe` Just (MonthDay 11)
      parseDay "21st" `shouldBe` Just (MonthDay 21)
      parseDay "31" `shouldBe` Just (MonthDay 31)
      parseDay "32" `shouldBe` Nothing
      parseDay "11st" `shouldBe` Nothing
      parseDay "unknown" `shouldBe` Nothing

    prop "roundtrips weekdays" $
      forAll (choose (1, 7)) $ \n ->
        case intToWeekDay n of
          Just day -> parseDay (showDay day) `shouldBe` Just day
          Nothing  -> expectationFailure "intToWeekDay rejected a valid day"

    prop "roundtrips month days" $
      forAll (choose (1, 31)) $ \n ->
        parseDay (showDay (MonthDay n)) `shouldBe` Just (MonthDay n)

  describe "parseEventKind" $ do
    it "parses event kinds" $ do
      parseEventKind "birthday" `shouldBe` Just EKBirthday
      parseEventKind "Getaway" `shouldBe` Just EKGetaway
      parseEventKind "noevent" `shouldBe` Just EKNoEvent
      parseEventKind "nope" `shouldBe` Nothing

    prop "roundtrips event kinds" $
      forAll (chooseEnum (minBound, maxBound)) $ \kind ->
        parseEventKind (showEventKind kind) `shouldBe` Just kind

  describe "extractEventType" $ do
    it "classifies events" $ do
      extractEventType (Just GenericEvent { eventName = "x", eventPeople = [] })
        `shouldBe` EKGeneric
      extractEventType (Just BirthdayEvent { eventName = "x", eventPeople = [] })
        `shouldBe` EKBirthday
      extractEventType (Just GetawayEvent { eventName = "x", eventPeople = [] })
        `shouldBe` EKGetaway
      extractEventType (Just GrandVacationEvent { eventName = "x", eventPeople = [] })
        `shouldBe` EKGrandVacation
      extractEventType (Just WorkTripEvent { eventName = "x", eventPeople = [] })
        `shouldBe` EKWorkTrip
      extractEventType Nothing `shouldBe` EKNoEvent

  describe "tests flash functionality" $ do
    describe "parseFlash" $ do
     it "parses aliases and rejects unknown" $ do
       parseFlash "int" `shouldBe` Just FlashInternal
       parseFlash "ext" `shouldBe` Just FlashExternal
       parseFlash "yes" `shouldBe` Just FlashAny
       parseFlash "unknown" `shouldBe` Nothing
       parseFlash "None" `shouldBe` Nothing

     prop "any valid flash value is parsed correctly" $
       forAll (chooseEnum (minBound, maxBound) `suchThat` (/= FlashUnknown)) $ \flash ->
         parseFlash (showFlash flash) `shouldBe` Just flash

    describe "formatFlashSource" $ do
     it "describes flash sources" $ do
       formatFlashSource FlashNone `shouldBe` "shot without flash"
       formatFlashSource FlashUnknown `shouldBe` "does not have flash information"
