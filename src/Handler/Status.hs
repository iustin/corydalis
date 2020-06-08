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
import qualified Formatting.ShortFormatters as F
import qualified Formatting.Time            as FT

import           Handler.Utils
import           Import
import           Pics

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

cleaning :: Text
cleaning = "Cleaning"

repoStatusToCardStyle :: RepoStatus -> Text
repoStatusToCardStyle RepoEmpty        = "border-warning"
repoStatusToCardStyle RepoStarting     = "border-primary"
repoStatusToCardStyle RepoScanning {}  = "border-primary"
repoStatusToCardStyle RepoRendering {} = "border-primary"
repoStatusToCardStyle RepoCleaning {}  = "border-primary"
repoStatusToCardStyle RepoFinished {}  = ""
repoStatusToCardStyle RepoError {}     = "border-danger text-danger"

repoStatusToScanStyle :: RepoStatus -> Text
repoStatusToScanStyle RepoEmpty        = "border-warning"
repoStatusToScanStyle RepoStarting     = "border-warning"
repoStatusToScanStyle RepoScanning {}  = "border-info"
repoStatusToScanStyle RepoRendering {} = "border-success"
repoStatusToScanStyle RepoCleaning {}  = "border-success"
repoStatusToScanStyle RepoFinished {}  = "border-succces"
repoStatusToScanStyle RepoError {}     = "border-danger text-danger"

repoStatusToRenderStyle :: RepoStatus -> Text
repoStatusToRenderStyle RepoEmpty        = "border-warning"
repoStatusToRenderStyle RepoStarting     = "border-warning"
repoStatusToRenderStyle RepoScanning {}  = "border-warning"
repoStatusToRenderStyle RepoRendering {} = "border-info"
repoStatusToRenderStyle RepoCleaning {}  = "border-success"
repoStatusToRenderStyle RepoFinished {}  = "border-succces"
repoStatusToRenderStyle RepoError {}     = "border-danger text-danger"

repoStatusToCleanStyle :: RepoStatus -> Text
repoStatusToCleanStyle RepoEmpty        = "border-warning"
repoStatusToCleanStyle RepoStarting     = "border-warning"
repoStatusToCleanStyle RepoScanning {}  = "border-warning"
repoStatusToCleanStyle RepoRendering {} = "border-warning"
repoStatusToCleanStyle RepoCleaning {}  = "border-info"
repoStatusToCleanStyle RepoFinished {}  = "border-succces"
repoStatusToCleanStyle RepoError {}     = "border-danger text-danger"

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

progressDetails :: Progress -> Int -> Widget
progressDetails counter goal =
  toWidget [hamlet|
                <ul>
                  <li>#{swissNum $ pgNoop counter} items were already up-to-date.
                  <li>#{swissNum $ pgDone counter} items needed processing.
                  <li>#{swissNum $ pgErrors counter} items had issues during processing.
                  <li>#{swissNum $ remaining} items to left to investigate.
                  |]
  where remaining = goal - pgTotal counter

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
          <div .card-body>
            <p .card-text>
               #{work} progress: #{swissNum (pgTotal counter)}/#{swissNum wsGoal}:
            ^{progressDetails counter wsGoal}
            <p .card-text>
               #{work} in progress for <abbr title="Since #{show wsStart}">#{relTime False delta}</abbr>.
               ETA: #{relTime True remaining}.
            ^{progressThroughput counter delta}
            ^{percentsBar counter wsGoal}
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
          <div .card-body>
            <p .card-text>
              #{work} finished, #{swissNum $ pgTotal wrDone} #{item} processed:
                 ^{progressDetails wrDone wrGoal}
            <p .card-text>
              #{work} started <abbr title="#{show wrStart}">#{relTime True (diffZ wrStart now)}</abbr>
              and took <abbr title="Ended at #{show wrEnd}">#{relTime False delta}</abbr>.
            ^{progressThroughput wrDone delta}
            ^{percentsBar wrDone wrGoal}
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

overallState :: RepoStatus -> (Int, Text, Text, Bool)
overallState RepoEmpty        = (0, "empty", "bg-warning", False)
overallState RepoStarting     = (5, "preparing scan", "bg-warning", False)
overallState RepoScanning {}  = (10, "scanning filesystem", "bg-info", True)
overallState RepoRendering {} = (50, "rendering images", "bg-info", True)
overallState RepoCleaning {}  = (90, "cleaning the cache", "bg-info", True)
overallState RepoFinished {}  = (100, "all done", "bg-info", False)
overallState RepoError {}     = (100, "error", "bg-danger", False)

getStatusR :: Handler Html
getStatusR = do
  repo <- getPics
  ctx <- getContext
  let repoState = repoStatus repo
  -- TODO: both of these should be moved to STM and read in a single
  -- transaction, and repo as well. [cleanup]
  scanProgress <- liftIO $ getProgress ctx
  renderProgress <- liftIO $ getRenderProgress ctx
  cleanProgress <- liftIO $ getCleanProgress ctx
  let (overall_perc, overall_text, overall_role, overall_strip) = overallState repoState
      overall_striptxt = if overall_strip then "progress-bar-striped" else ""::Text
  now <- liftIO getZonedTime
  defaultLayout $ do
    setHtmlTitle "status"
    $(widgetFile "status")
