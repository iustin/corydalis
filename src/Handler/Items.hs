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

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoCPP                 #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Handler.Items
  ( itemDiv
  , allPicsDiv
  , symbolPlCap
  ) where

import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.LocalTime (LocalTime (localDay), getTimeZone,
                                      utcToLocalTime)
import           Exif                (formatPerson)
import           Handler.Utils
import           Import
import           Indexer
import           Pics                (Image (..))

symbolPlCap :: Symbol -> Text
symbolPlCap TCountry      = "Countries"
symbolPlCap TProvince     = "Provinces"
symbolPlCap TCity         = "Cities"
symbolPlCap TLocation     = "Locations"
symbolPlCap TPerson       = "People"
symbolPlCap TKeyword      = "Keywords"
symbolPlCap TTitle        = "Image titles"
symbolPlCap TCaption      = "Image captions"
symbolPlCap TProblem      = "Problems"
symbolPlCap TYear         = "Years"
symbolPlCap TSeason       = "Seasons"
symbolPlCap TMonth        = "Months"
symbolPlCap TDay          = "Days"
symbolPlCap TCamera       = "Cameras"
symbolPlCap TLens         = "Lenses"
symbolPlCap TFStop        = "F-stops"
symbolPlCap TShutterSpeed = "Shutter speeds"
symbolPlCap TIso          = "ISO values"
symbolPlCap TFocalLength  = "Focal lengths"
symbolPlCap TType         = "Types"
symbolPlCap TFolder       = "Folders"
symbolPlCap TFileName     = "File names"
symbolPlCap TStatus       = "Image status"
symbolPlCap TFClass       = "Folder class"
symbolPlCap TRating       = "Image ratings"
symbolPlCap TPplCnt       = "People count"
symbolPlCap TKwdCnt       = "Keyword count"
symbolPlCap TFlashSrc     = "Flash source"
symbolPlCap TFlashMode    = "Flash information"
symbolPlCap TMegapixels   = "Megapixels"

divClassForNoAtom :: Symbol -> Text
divClassForNoAtom TProblem = "border-info"
divClassForNoAtom _        = "border-warning"

divClassForAtom :: Symbol -> Text
divClassForAtom TProblem = "border-warning"
divClassForAtom _        = ""

formatter :: Symbol -> (Text -> Text)
formatter TPerson = formatPerson True
formatter _       = id

localToday :: IO Day
localToday = do
  utc <- getCurrentTime
  zone <- getTimeZone utc
  let localTime = utcToLocalTime zone utc
  return . localDay $ localTime

itemHeader :: Symbol -> Widget
itemHeader symbol =
  [whamlet|
          <div .card-header .py-2>
            <span class="#{atomIcon symbol} fa-fw">
            #{symbolPlCap symbol}
          |]

itemDiv :: Symbol -> Bool -> ([Text], [Text]) -> Widget
itemDiv symbol buttons (items, length -> rcount) = do
  let dclass = if null items
               then divClassForNoAtom
               else divClassForAtom
  $(widgetFile "itemdiv")

allPicsDiv :: Maybe Image -> Widget
allPicsDiv randomImage = do
  today <- liftIO localToday
  let (_, thisMonth, thisDay) = toGregorian today
      dow = dayOfWeek today
      -- It's unlikely that we can't resolve the month, but…
      thisMonthOp = fromMaybe Indexer.January (intToMonth thisMonth)
      onThisDate = atomToParams $ And (Month thisMonthOp) (Day (MonthDay thisDay))
      -- We're lucky here that the Data.Time.Calendar days of week enum
      -- indices are the same as ours.
      onThisDoW = atomToParams . Day . fromMaybe Indexer.Monday . intToWeekDay . fromEnum $ dow
  $(widgetFile "alldiv")
