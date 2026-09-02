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

{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- | Base atom value types shared between search indexing and repository
-- statistics.
--
-- These types are deliberately independent of 'Pics' so that picture
-- metadata can be classified (season, month, day, flash, event kind)
-- while the repository is built.

module AtomTypes ( SeasonOp(..)
                 , MonthOp(..)
                 , DayOp(..)
                 , FlashOp(..)
                 , EventKindOp(..)
                 , showSeason
                 , showMonth
                 , showDay
                 , showFlash
                 , showEventKind
                 , intToMonth
                 , intToWeekDay
                 , weekdayToEnd
                 , monthToSeason
                 , showOrdinal
                 , parseFlash
                 , parseSeason
                 , parseMonth
                 , parseDay
                 , parseEventKind
                 , formatFlashSource
                 , extractEventType
                 ) where

import           Control.DeepSeq
import           Data.Store              (Store)
import qualified Data.Text               as Text
import qualified Data.Text.Read          as Text

import           Import.NoFoundation     hiding (leftover)
import           Utils.Parsing           (parseDecimal)

data SeasonOp
  = Spring
  | Summer
  | Autumn
  | Winter
  | SeasonUnknown
    deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Store SeasonOp

instance NFData SeasonOp where
  rnf = rwhnf

data MonthOp
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  | MonthUnknown
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Store MonthOp

instance NFData MonthOp where
  rnf = rwhnf

data DayOp
  = Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday
  | Saturday
  | Sunday
  | Weekday
  | Weekend
  | MonthDay Int
  | DayUnknown
  deriving (Show, Eq, Ord, Generic)

instance Store DayOp

instance NFData DayOp where
  rnf (MonthDay n) = rnf n
  rnf x            = rwhnf x

data FlashOp
 = FlashNone
 | FlashInternal
 | FlashExternal
 | FlashAny
 | FlashUnknown
 deriving (Show, Eq, Enum, Bounded, Ord, Generic)

instance Store FlashOp

instance NFData FlashOp where
  rnf = rwhnf

data EventKindOp
  = EKGeneric
  | EKBirthday
  | EKGetaway
  | EKGrandVacation
  | EKWorkTrip
  | EKNoEvent
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

instance Store EventKindOp

instance NFData EventKindOp where
  rnf = rwhnf

parseFlash :: Text -> Maybe FlashOp
parseFlash v
  | v == "none"                    = Just FlashNone
  | v == "internal" || v == "int"  = Just FlashInternal
  | v == "external" || v == "ext"  = Just FlashExternal
  | v == "yes" || v == "any"       = Just FlashAny
  | otherwise                      = Nothing

showFlash :: FlashOp -> Text
showFlash FlashNone     = "none"
showFlash FlashInternal = "internal"
showFlash FlashExternal = "external"
showFlash FlashAny      = "any"
showFlash FlashUnknown  = "unknown"

parseSeason :: Text -> Maybe SeasonOp
parseSeason (Text.toLower -> s)
  | s == "winter" = Just Winter
  | s == "spring" = Just Spring
  | s == "summer" = Just Summer
  | s == "autumn" = Just Autumn
  | otherwise     = Nothing

showSeason :: SeasonOp -> Text
showSeason Winter        = "winter"
showSeason Spring        = "spring"
showSeason Summer        = "summer"
showSeason Autumn        = "autumn"
showSeason SeasonUnknown = "unknown"

intToMonth :: Int -> Maybe MonthOp
intToMonth 1  = Just January
intToMonth 2  = Just February
intToMonth 3  = Just March
intToMonth 4  = Just April
intToMonth 5  = Just May
intToMonth 6  = Just June
intToMonth 7  = Just July
intToMonth 8  = Just August
intToMonth 9  = Just September
intToMonth 10 = Just October
intToMonth 11 = Just November
intToMonth 12 = Just December
intToMonth _  = Nothing

parseMonth :: Text -> Maybe MonthOp
parseMonth (Text.toLower -> m)
  | m == "january"   = Just January
  | m == "february"  = Just February
  | m == "march"     = Just March
  | m == "april"     = Just April
  | m == "may"       = Just May
  | m == "june"      = Just June
  | m == "july"      = Just July
  | m == "august"    = Just August
  | m == "september" = Just September
  | m == "october"   = Just October
  | m == "november"  = Just November
  | m == "december"  = Just December
  | otherwise =
      either (const Nothing) intToMonth $ parseDecimal m

showMonth :: MonthOp -> Text
showMonth MonthUnknown = "unknown"
showMonth s            = sformat shown s

-- FIXME: replace with ords when newer formatting library (no longer
-- .0 bug) [dependency].
showOrdinal :: (Integral a) => a -> Text
showOrdinal n
  | n < 0 = sformat int n
  | tens > 3 && tens < 21 = sformat int n <> "th"
  | otherwise =
      sformat int n <>
      case n `mod` 10 of
        1 -> "st"
        2 -> "nd"
        3 -> "rd"
        _ -> "th"
  where tens = n `mod` 100

-- | Simpler Text to ordinal parsing with error handling.
--
-- It accepts usual prefixes such as 'th', 'st', 'nd', 'rd', as long as they're valid.
parseOrdinal :: (Integral a) => Text -> Either Text a
parseOrdinal w =
  case Text.decimal w of
    Right (w', "") -> Right w'
    Right (w', suff) | w == showOrdinal w' &&
                       (suff == "th" ||
                        suff == "st" ||
                        suff == "nd" ||
                        suff == "rd") -> Right w'
    Right (w', leftover) ->
      Left $ sformat ("Parsed " % int % " decimal but with leftover text '" %
                      stext % "'") w' leftover
    Left msg ->
      Left $ sformat ("Failed to parse integer from '" % stext % "': " %
                      string) w msg

parseDay :: Text -> Maybe DayOp
parseDay (Text.toLower -> d)
  | d == "monday"    = Just Monday
  | d == "tuesday"   = Just Tuesday
  | d == "wednesday" = Just Wednesday
  | d == "thursday"  = Just Thursday
  | d == "friday"    = Just Friday
  | d == "saturday"  = Just Saturday
  | d == "sunday"    = Just Sunday
  | d == "weekday"   = Just Weekday
  | d == "weekend"   = Just Weekend
  | otherwise =
      case parseOrdinal d of
        Right v | v >= 1 && v <= 31 -> Just $ MonthDay v
        _                           -> Nothing

showDay :: DayOp -> Text
showDay Monday       = "Monday"
showDay Tuesday      = "Tuesday"
showDay Wednesday    = "Wednesday"
showDay Thursday     = "Thursday"
showDay Friday       = "Friday"
showDay Saturday     = "Saturday"
showDay Sunday       = "Sunday"
showDay Weekday      = "weekday"
showDay Weekend      = "weekend"
showDay (MonthDay d) = showOrdinal d
showDay DayUnknown   = "unknown"

intToWeekDay :: Int -> Maybe DayOp
intToWeekDay 1 = Just Monday
intToWeekDay 2 = Just Tuesday
intToWeekDay 3 = Just Wednesday
intToWeekDay 4 = Just Thursday
intToWeekDay 5 = Just Friday
intToWeekDay 6 = Just Saturday
intToWeekDay 7 = Just Sunday
intToWeekDay _ = Nothing

-- | Converts a Day into another Day representing weekend or not.
--
-- Ordinal month days will be classified as weekday, sadly. This
-- points to some lack of soundness in the argument.
weekdayToEnd :: DayOp -> DayOp
weekdayToEnd Saturday = Weekend
weekdayToEnd Sunday   = Weekend
weekdayToEnd _        = Weekday

-- | Computes the season based on a month.
--
-- Note that the definition of season is currently hardcoded to
-- month-boundaries, not based on equinox, etc.
monthToSeason :: MonthOp -> Maybe SeasonOp
monthToSeason m
  | m == December || m == January || m == February = Just Winter
  | m == March || m == April || m == May = Just Spring
  | m == June || m == July || m == August = Just Summer
  | m == September || m == October || m == November = Just Autumn
  | otherwise = Nothing -- FIXME: is this needed?

showEventKind :: EventKindOp -> Text
showEventKind EKGeneric       = "generic"
showEventKind EKBirthday      = "birthday"
showEventKind EKGetaway       = "getaway"
showEventKind EKGrandVacation = "grandvacation"
showEventKind EKWorkTrip      = "worktrip"
showEventKind EKNoEvent       = "noevent"

parseEventKind :: Text -> Maybe EventKindOp
parseEventKind (Text.toLower -> v)
  | v == "generic"       = Just EKGeneric
  | v == "birthday"      = Just EKBirthday
  | v == "getaway"       = Just EKGetaway
  | v == "grandvacation" = Just EKGrandVacation
  | v == "worktrip"      = Just EKWorkTrip
  | v == "noevent"       = Just EKNoEvent
  | otherwise            = Nothing

formatFlashSource :: FlashOp -> Text
formatFlashSource FlashNone     = "shot without flash"
formatFlashSource FlashInternal = "shot with internal flash"
formatFlashSource FlashExternal = "shot with an external flash"
formatFlashSource FlashAny      = "shot with an active flash (any type)"
formatFlashSource FlashUnknown  = "does not have flash information"

extractEventType :: Maybe Event -> EventKindOp
extractEventType (Just GenericEvent {})       = EKGeneric
extractEventType (Just BirthdayEvent {})      = EKBirthday
extractEventType (Just GetawayEvent {})       = EKGetaway
extractEventType (Just GrandVacationEvent {}) = EKGrandVacation
extractEventType (Just WorkTripEvent {})      = EKWorkTrip
extractEventType Nothing                      = EKNoEvent
