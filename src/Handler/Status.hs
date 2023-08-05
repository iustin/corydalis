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

progressDetails :: Progress -> Widget
progressDetails counter =
  toWidget [hamlet|
                <ul>
                  <li>#{swissNumOrNone $ pgNoop counter} items were already up-to-date.
                  <li>#{swissNumOrNone $ pgDone counter} items needed processing.
                  <li>
                    #{swissNumOrNone $ pgNumErrors counter} items had #
                    <a href=@{StatusErrorsR}>
                      issues
                    \ during processing.
                  <li>#{swissNumOrNone $ remaining} items to left to investigate.
                  |]
  where remaining = pgGoal counter - pgTotal counter

progressThroughput :: Progress -> NominalDiffTime -> Widget
progressThroughput counter delta =
  toWidget [hamlet|
            <p .card-text>
              Throughput: #{showThroughput $ throughput (pgTotal counter) delta} files/s overall,
              #{showThroughput $ throughput totalWork delta} files/s for actual work.
              |]
  where totalWork = pgDone counter + pgNumErrors counter

workInProgress :: ZonedTime -> Text -> Progress -> WorkStart -> Widget
workInProgress now work counter@Progress{..} WorkStart{..} =
  [whamlet|
          <div .card-body>
            <p .card-text>
               #{work} progress: #{swissNum (pgTotal counter)}/#{swissNum pgGoal}:
            ^{progressDetails counter}
            <p .card-text>
               #{work} in progress for <abbr title="Since #{show wsStart}">#{relTime False delta}</abbr>.
               ETA: #{relTime True remaining}.
            ^{progressThroughput counter delta}
            ^{percentsBar counter}
               |]
  where doneitems = pgTotal counter - pgNoop
        -- Tricky: if we actually did work, estimate on (goal - noop)
        -- / actual work. If not, then fall back to goal /
        -- work. Otherwise, with the formaer we'd never get an ETA for
        -- all-cached scenario, and with the latter, we'd get overly
        -- optimistic estimations in the partially-cached case. Of
        -- course, can still show get inf right at the start, but
        -- that's acceptable.
        multiplier = if doneitems > 0
                     then fromIntegral (pgGoal - pgNoop) /
                          fromIntegral doneitems::Double
                     else fromIntegral pgGoal / fromIntegral (pgTotal counter)
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
                 ^{progressDetails wrDone}
            <p .card-text>
              #{work} started <abbr title="#{show wrStart}">#{relTime True (diffZ wrStart now)}</abbr>
              and took <abbr title="Ended at #{show wrEnd}">#{relTime False delta}</abbr>.
            ^{progressThroughput wrDone delta}
            ^{percentsBar wrDone}
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

percentsDone :: Progress -> (Int, Int, Int, Int)
percentsDone p@Progress{..} =
  -- Normalisation for total: if total < pgTotal p, then take the
  -- latter as goal (some weird error in this case). If that's still 0
  -- (in 0/0 case), make it 1 to not have to deal with ±∞.
  let atotal = fromIntegral (maximumEx [pgGoal, pgTotal p, 1])::Double
      f x = truncate $ fromIntegral x * 100 / atotal
      pE = f (pgNumErrors p)
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

percentsBar :: Progress -> Widget
percentsBar counter =
  [whamlet|
    <div .progress>
      ^{pgBar pN "bg-success" "Already up-to-date"}
      ^{pgBar pE "bg-danger" "Processed with errors"}
      ^{pgBar pD "bg-info progress-bar-striped" "Processed successfully"}
      ^{pgBar pR "bg-light text-secondary" "Left to do"}
      |]
  where (pE, pN, pD, pR) = percentsDone counter

readProgresses :: Ctx -> STM (Progress, Progress, Progress)
readProgresses ctx = do
  scan <- readTVar $ ctxScanProgress ctx
  render <- readTVar $ ctxRenderProgress ctx
  clean <- readTVar $ ctxCleanProgress ctx
  return (scan, render, clean)

percentBetween :: Progress -> Int -> Int -> Int
percentBetween p low high
  | Just perc <- pgProgress p =
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
overallState RepoScanning {}  s _ _ = (percentBetween s scanStart renderStart,
                                       "scanning filesystem", "bg-info", True)
overallState RepoRendering {} _ r _ = (percentBetween r renderStart cleanStart,
                                       "rendering images", "bg-info", True)
overallState RepoCleaning {}  _ _ c = (percentBetween c cleanStart 100,
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
