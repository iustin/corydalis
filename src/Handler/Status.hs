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
  , getStatusErrorsR
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

swissNumOrNone :: (Integral n, Buildable n) => n -> Text
swissNumOrNone 0 = "no"
swissNumOrNone n = swissNum n

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
      Repository currently contains #{swissNumOrNone totalImages} images.
    <p .card-text>
      Repository generation number is #{repoSerial repo}.
      |]
  where totalImages = totalStatsCount . rsPicStats . repoStats $ repo

data ProgressKind = InvestigateAll | WorkOnly

-- | Items that count toward the displayed N/M fraction.
countedItems :: ProgressKind -> Progress -> Int
countedItems InvestigateAll = pgTotal
countedItems WorkOnly       = pgWork

remainingItems :: ProgressKind -> Progress -> Int
remainingItems kind p = pgGoal p - countedItems kind p

progressDetails :: ProgressKind -> Progress -> Widget
progressDetails kind counter =
  toWidget [hamlet|
                <ul>
                  <li>#{swissNumOrNone $ pgNoop counter} items were already up-to-date.
                  <li>#{swissNumOrNone $ pgDone counter} items needed processing.
                  <li>
                    #{swissNumOrNone $ pgNumErrors counter} items had #
                    <a href=@{StatusErrorsR}>
                      issues
                    \ during processing.
                  <li>#{swissNumOrNone $ remainingItems kind counter} #{remainingLabel}
                  |]
  where remainingLabel = case kind of
          InvestigateAll -> "items to left to investigate." :: Text
          WorkOnly       -> "items left to process."

progressThroughput :: ProgressKind -> Progress -> NominalDiffTime -> Widget
progressThroughput InvestigateAll counter delta =
  toWidget [hamlet|
            <p .card-text>
              Throughput: #{showThroughput $ throughput (pgTotal counter) delta} files/s overall,
              #{showThroughput $ throughput (pgWork counter) delta} files/s for actual work.
              |]
progressThroughput WorkOnly counter delta =
  toWidget [hamlet|
            <p .card-text>
              Throughput: #{showThroughput $ throughput (pgWork counter) delta} files/s.
              |]

workInProgress :: ZonedTime -> Text -> Progress -> WorkStart -> ProgressKind -> Widget
workInProgress now work counter@Progress{..} WorkStart{..} kind =
  [whamlet|
          <div .card-body>
            <p .card-text>
               #{work} progress: #{swissNum $ countedItems kind counter}/#{swissNum pgGoal}:
            ^{progressDetails kind counter}
            <p .card-text>
               #{work} in progress for <abbr title="Since #{show wsStart}">#{relTime False delta}</abbr>.
               ETA: #{relTime True remaining}.
            ^{progressThroughput kind counter delta}
            ^{percentsBar kind counter}
               |]
  where doneitems = pgWork counter
        -- Tricky: if we actually did work, estimate on (goal - noop)
        -- / actual work. If not, then fall back to goal /
        -- work. Otherwise, with the former we'd never get an ETA for
        -- all-cached scenario, and with the latter, we'd get overly
        -- optimistic estimations in the partially-cached case. Of
        -- course, can still show inf right at the start, but
        -- that's acceptable. WorkOnly goals already exclude noops.
        multiplier :: Double
        multiplier = case kind of
          InvestigateAll ->
            if doneitems > 0
            then fromIntegral (pgGoal - pgNoop) / fromIntegral doneitems
            else fromIntegral pgGoal / fromIntegral (pgTotal counter)
          WorkOnly ->
            if doneitems > 0
            then fromIntegral pgGoal / fromIntegral doneitems
            else if pgGoal == 0 then 1 else 1/0
        elapsed = realToFrac $ diffZ now wsStart
        totaltime = elapsed * multiplier
        remaining = totaltime - elapsed
        delta = diffZ now wsStart
        -- TODO: add actual ETA once upgrading to newer time library [easy] [dependency].

workResults :: ZonedTime -> WorkResults -> Text -> Text -> ProgressKind -> Widget
workResults now WorkResults{..} work item kind =
  [whamlet|
          <div .card-body>
            <p .card-text>
              #{work} finished, #{swissNum $ countedItems kind wrDone} #{item} processed:
                 ^{progressDetails kind wrDone}
            <p .card-text>
              #{work} started <abbr title="#{show wrStart}">#{relTime True (diffZ wrStart now)}</abbr>
              and took <abbr title="Ended at #{show wrEnd}">#{relTime False delta}</abbr>.
            ^{progressThroughput kind wrDone delta}
            ^{percentsBar kind wrDone}
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

percentsDone :: ProgressKind -> Progress -> (Int, Int, Int, Int)
percentsDone WorkOnly Progress{..}
  | pgGoal <= 0 = (0, 0, 100, 0)
percentsDone kind p@Progress{..} =
  -- Normalisation for total: if total < counted, then take the
  -- latter as goal (some weird error in this case). If that's still 0
  -- (in 0/0 case), make it 1 to not have to deal with ±∞.
  let counted = countedItems kind p
      atotal = fromIntegral (maximumEx [pgGoal, counted, 1])::Double
      f x = truncate $ fromIntegral x * 100 / atotal
      pE = f (pgNumErrors p)
      pN = case kind of
        InvestigateAll -> f pgNoop
        WorkOnly       -> 0
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

percentsBar :: ProgressKind -> Progress -> Widget
percentsBar kind counter =
  [whamlet|
    <div .progress>
      ^{pgBar pN "bg-success" "Already up-to-date"}
      ^{pgBar pE "bg-danger" "Processed with errors"}
      ^{pgBar pD "bg-info progress-bar-striped" "Processed successfully"}
      ^{pgBar pR "bg-light text-secondary" "Left to do"}
      |]
  where (pE, pN, pD, pR) = percentsDone kind counter

readProgresses :: Ctx -> STM (Progress, Progress, Progress)
readProgresses ctx = do
  scan <- readTVar $ ctxScanProgress ctx
  render <- readTVar $ ctxRenderProgress ctx
  clean <- readTVar $ ctxCleanProgress ctx
  return (scan, render, clean)

percentBetween :: (Progress -> Maybe Double) -> Progress -> Int -> Int -> Int
percentBetween prog p low high
  | Just perc <- prog p =
      truncate (fromIntegral (high - low) * perc) + low
  | otherwise = low

scanStart :: Int
scanStart = 10

renderStart :: Int
renderStart = 50

cleanStart :: Int
cleanStart = 90

overallState :: RepoStatus -> Progress -> Progress -> Progress
             -> (Int, Text, Text, Bool)
overallState RepoEmpty        _ _ _  = (0, "empty", "bg-warning", False)
overallState RepoStarting     _ _ _  = (5, "preparing scan", "bg-warning", False)
overallState RepoScanning {}  s _ _ = (percentBetween pgProgress s scanStart renderStart,
                                       "scanning filesystem", "bg-info", True)
overallState RepoRendering {} _ r _ = (percentBetween pgWorkProgress r renderStart cleanStart,
                                       "rendering images", "bg-info", True)
overallState RepoCleaning {}  _ _ c = (percentBetween pgProgress c cleanStart 100,
                                       "cleaning the cache", "bg-info", True)
overallState RepoFinished {}  _ _ _ = (100, "all done", "bg-info", False)
overallState RepoError {}     _ _ _ = (100, "error", "bg-danger", False)

repoScanProgress :: Ctx -> RepoStatus -> Widget
repoScanProgress ctx repoState = do
  (sp, rp, cp) <- liftIO $ atomically $ readProgresses ctx
  let (overall_perc, overall_text, overall_role, overall_strip) = overallState repoState sp rp cp
      overall_striptxt = if overall_strip then "progress-bar-striped" else ""::Text
  $(widgetFile "scanprogress")

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
  now <- liftIO getZonedTime
  defaultLayout $ do
    setHtmlTitle "status"
    $(widgetFile "status")

getStatusErrorsR :: Handler Html
getStatusErrorsR = do
  ctx <- getContext
  (finished, repoState, sp, rp, cp) <- liftIO $ atomically $ do
    repo <- readTVar (ctxRepo ctx)
    let repoState = repoStatus repo
    (instScan, instRend, instClean) <- readProgresses ctx
    return $ case repoStatus repo of
      RepoFinished { rsScanResults = finScan
                   , rsRenderResults = finRend
                   , rsCleanResults = finClean
                   } -> (True, repoState,
                         wrDone finScan, wrDone finRend, wrDone finClean)
      RepoCleaning { rsScanResults = finScan
                   , rsRenderResults = finProg
                   } -> (False, repoState,
                         wrDone finScan, wrDone finProg, instClean)

      RepoRendering { rsScanResults = finScan
                    } -> (False, repoState,
                          wrDone finScan, instRend, def)
      RepoScanning {} -> (False, repoState, instScan, def, def)
      _ -> (False, repoState, def, def, def)
  let errors = map (scanning,)  (pgErrors sp) ++
               map (rendering,) (pgErrors rp) ++
               map (cleaning,)  (pgErrors cp)
  defaultLayout $ do
    setHtmlTitle "Repository scanning errors"
    $(widgetFile "progressinfo")
