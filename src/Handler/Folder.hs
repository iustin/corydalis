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

module Handler.Folder
  ( getFolderR
  ) where

import           Exif
import           Handler.Utils
import           Handler.Widgets
import           Import
import           Indexer         (Symbol (TBirthday, TEvent, TGetaway, TGrandVacation, TPerson, TWorkTrip),
                                  symbolName)
import           Pics

import qualified Data.Map        as Map
import qualified Data.Set        as Set
import qualified Data.Text.Short as TS

eventKindInfo :: Event -> (Text, Symbol)
eventKindInfo GenericEvent{}       = ("Event", TEvent)
eventKindInfo BirthdayEvent{}      = ("Birthday", TBirthday)
eventKindInfo GetawayEvent{}       = ("Getaway", TGetaway)
eventKindInfo GrandVacationEvent{} = ("Grand vacation", TGrandVacation)
eventKindInfo WorkTripEvent{}      = ("Work trip", TWorkTrip)

showFolderEvent :: Maybe Event -> Widget
showFolderEvent Nothing =
  [whamlet|This folder is not associated with an event.|]
showFolderEvent (Just ev) = do
  let (kindLabel, kindSymbol) = eventKindInfo ev
      kindIcon = atomIcon kindSymbol
      eventNameText = TS.toText (eventName ev)
      people = eventPeople ev
  [whamlet|
    <div .mb-2>
      <span>
        <span>
          <span class="#{kindIcon}" aria-hidden=true>
        #{kindLabel}:
      <span>#{eventNameText}

    $if null people
      <div .text-muted>No people annotated for this event.
    $else
      $forall p <- people
        $with pText <- TS.toText p
          $with searchParams <- [(symbolName kindSymbol, pText)]
            <a .btn .btn-light .btn-sm .me-1 .mb-1 href="@?{(SearchR,searchParams)}">
              #{formatPerson True p}

    $case eventSource ev
      $of EventImplicit
        <div .text-muted>This event was inferred from the folder contents.
      $of EventExplicit mPath
        $maybe path <- mPath
          <div .text-muted>This event was explicitly defined in #
            <span .monolight>#{quoteMarkup path}
            .
        $nothing
          <div .text-muted>This event was explicitly defined, but the source is unknown.
  |]

getFolderR :: ShortText -> Handler Html
getFolderR name = do
  config <- getConfig
  (pics, dir) <- getPicsAndFolder name
  params <- getParams
  let allpaths = pdMainPath dir:pdSecPaths dir
      thumbsize = cfgThumbnailSize config
  defaultLayout $ do
    let stats = computeFolderStats dir
        people = sPeople stats
        daterange = sDateRange stats
        fc = folderClassFromStats stats
        images = map snd . Map.toList $ pdImages dir
        exifs = map imgExif images
        cameras = countItems . map (fmap deSymbolizeItem' . exifCamera) $ exifs
        lenses = countItems . map exifLens $ exifs
        event = pdEvent dir
    setHtmlTitle $ "folder " <> TS.toText name
    $(widgetFile "folder")
