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

module Handler.Status
  ( getStatusR
  ) where

import           Data.Time.Clock
import           Data.Time.LocalTime
import           Formatting                 (Buildable, groupInt, sformat)
import qualified Formatting.ShortFormatters as F
import qualified Formatting.Time            as FT

import           Handler.Utils
import           Import
import           Pics
import           Types

throughput :: Int -> NominalDiffTime -> Double
throughput t delta = (fromIntegral t::Double) / realToFrac delta

showThroughput :: Double -> Text
showThroughput = sformat (F.f 2)

diffZ :: ZonedTime -> ZonedTime -> NominalDiffTime
diffZ a b = diffUTCTime (zonedTimeToUTC a) (zonedTimeToUTC b)

swissNum :: (Integral n, Buildable n) => n -> Text
swissNum = sformat (groupInt 3 '\'')

relTime :: RealFrac n => Bool -> n -> Text
relTime b = sformat (FT.diff b)

scanning :: Text
scanning = "Scanning"

rendering :: Text
rendering = "Rendering"

repoStatusToCardStyle :: RepoStatus -> Text
repoStatusToCardStyle RepoEmpty        = "border-warning"
repoStatusToCardStyle RepoStarting     = "border-primary"
repoStatusToCardStyle RepoScanning {}  = "border-primary"
repoStatusToCardStyle RepoRendering {} = "border-primary"
repoStatusToCardStyle RepoFinished {}  = ""
repoStatusToCardStyle RepoError {}     = "border-danger text-danger"

repoStatusToScanStyle :: RepoStatus -> Text
repoStatusToScanStyle RepoEmpty        = "border-warning"
repoStatusToScanStyle RepoStarting     = "border-warning"
repoStatusToScanStyle RepoScanning {}  = "border-info"
repoStatusToScanStyle RepoRendering {} = "border-success"
repoStatusToScanStyle RepoFinished {}  = "border-succces"
repoStatusToScanStyle RepoError {}     = "border-danger text-danger"

repoStatusToRenderStyle :: RepoStatus -> Text
repoStatusToRenderStyle RepoEmpty        = "border-warning"
repoStatusToRenderStyle RepoStarting     = "border-warning"
repoStatusToRenderStyle RepoScanning {}  = "border-warning"
repoStatusToRenderStyle RepoRendering {} = "border-info"
repoStatusToRenderStyle RepoFinished {}  = "border-succces"
repoStatusToRenderStyle RepoError {}     = "border-danger text-danger"

repoInProgress :: ZonedTime -> Text -> WorkStart -> Widget
repoInProgress now work WorkStart{..} =
  toWidget [hamlet|
            <p .card-text>
               #{work} in progress for <abbr title="Since #{show wsStart}">#{relTime False (diffZ now wsStart)}</abbr>.
               |]

repoContents :: Repository -> Widget
repoContents repo =
  toWidget [hamlet|
    <p .card-text>
      Repository currently contains #
      $if totalImages > 0
        #{swissNum totalImages} images
      $else
        no images
      .
    <p .card-text>
      Repository generation number is #{repoSerial repo}.
      |]
  where totalImages = totalStatsCount . rsPicStats . repoStats $ repo

progressDetails :: Progress -> Widget
progressDetails counter =
  toWidget [hamlet|
                <ul>
                  <li>#{swissNum $ pgNoop counter} items were already up-to-date.
                  <li>#{swissNum $ pgDone counter} items needed processing.
                  <li>#{swissNum $ pgErrors counter} items had issues during processing.
                  |]

progressThroughput :: Progress -> NominalDiffTime -> Widget
progressThroughput counter delta =
  toWidget [hamlet|
            <p .card-text>
              Throughput: #{showThroughput $ throughput (pgTotal counter) delta} files/s overall,
              #{showThroughput $ throughput totalWork delta} files/s for actual work.
              |]
  where totalWork = pgDone counter + pgErrors counter

workInProgress :: ZonedTime -> Text -> Progress -> WorkStart -> Widget
workInProgress now work counter WorkStart{..} =
  [whamlet|
            <p .card-text>
               #{work} progress: #{swissNum (pgTotal counter)}/#{swissNum wsGoal}:
            ^{progressDetails counter}
            <p .card-text>
               #{work} in progress for <abbr title="Since #{show wsStart}">#{relTime False delta}</abbr>.
               ETA: #{relTime True remaining}.
            ^{progressThroughput counter delta}
               |]
  where doneitems = pgTotal counter - pgNoop counter
        -- Tricky: if we actually did work, estimate on (goal - noop)
        -- / actual work. If not, then fall back to goal /
        -- work. Otherwise, with the formaer we'd never get an ETA for
        -- all-cached scenario, and with the latter, we'd get overly
        -- optimistic estimations in the partially-cached case. Of
        -- course, can still show get inf right at the start, but
        -- that's acceptable.
        multiplier = if doneitems > 0
                     then fromIntegral (wsGoal - pgNoop counter) /
                          fromIntegral doneitems::Double
                     else fromIntegral wsGoal / fromIntegral (pgTotal counter)
        elapsed = realToFrac $ diffZ now wsStart
        totaltime = elapsed * multiplier
        remaining = totaltime - elapsed
        delta = diffZ now wsStart
        -- TODO: add actual ETA once upgrading to newer time library [easy] [dependency].

workResults :: ZonedTime -> WorkResults -> Text -> Text -> Widget
workResults now WorkResults{..} work item =
  [whamlet|
            <p .card-text>
              #{work} finished, #{swissNum $ pgTotal wrDone} #{item} processed:
                 ^{progressDetails wrDone}
            <p .card-text>
              #{work} started <abbr title="#{show wrStart}">#{relTime True (diffZ wrStart now)}</abbr>
              and took <abbr title="Ended at #{show wrEnd}">#{relTime False delta}</abbr>.
            ^{progressThroughput wrDone delta}
              |]
  where delta = diffZ wrEnd wrStart

workIdle :: Text -> Widget
workIdle work =
  toWidget [hamlet|
          <div .card-body .text-warning>
            #{work} has not started yet.
            |]

scanFailed :: Widget
scanFailed =
  toWidget [hamlet|
          <div .card-body .text-danger>
            Repository scanning failed.
            |]

percentsDone :: Progress -> Int -> (Int, Int, Int, Int)
percentsDone p@Progress{..} total =
  -- Normalisation for total: if total < pgTotal p, then take the
  -- latter as goal (some weird error in this case). If that's still 0
  -- (in 0/0 case), make it 1 to not have to deal with ±∞.
  let atotal = fromIntegral (maximumEx [total, pgTotal p, 1])::Double
      f x = truncate $ fromIntegral x * 100 / atotal
      pE = f pgErrors
      pN = f pgNoop
      pD = f pgDone
      pR = 100 - pE - pN - pD
  in (pE, pN, pD, pR)

pgBar :: Int -> Text -> Text-> Widget
pgBar perc classes title =
  toWidget [hamlet|
      <div class="progress-bar #{classes}"
           role=progressbar
           aria-valuenow="#{perc}" aria-valuemin="0" aria-valuemax="100"
           style="width: #{perc}%" title="#{title}">
        #{perc}%
        |]

percentsBar :: Progress -> Int -> Widget
percentsBar counter goal =
  [whamlet|
    <div .progress>
      ^{pgBar pN "bg-success" "Already up-to-date"}
      ^{pgBar pE "bg-danger" "Processed with errors"}
      ^{pgBar pD "bg-info progress-bar-striped" "Processed successfully"}
      ^{pgBar pR "bg-light text-secondary" "Left to do"}
      |]
  where (pE, pN, pD, pR) = percentsDone counter goal

getStatusR :: Handler Html
getStatusR = do
  repo <- getPics
  let repoState = repoStatus repo
  scanProgress <- liftIO getProgress
  renderProgress <- liftIO getRenderProgress
  let (overall_perc, overall_text, overall_role, overall_strip) = case repoState of
        RepoEmpty         -> (0::Int, "empty"::Text, "bg-warning"::Text, False)
        RepoStarting      -> (5, "preparing scan", "bg-warning", False)
        RepoScanning _    -> (10, "scanning filesystem", "bg-info", True)
        RepoRendering _ _ -> (50, "rendering images", "bg-info", True)
        RepoFinished _ _  -> (100, "all done", "bg-info", False)
        RepoError _       -> (100, "error", "bg-danger", False)
      overall_striptxt = if overall_strip then "progress-bar-striped" else ""::Text
  now <- liftIO getZonedTime
  defaultLayout $ do
    setHtmlTitle "status"
    $(widgetFile "status")
