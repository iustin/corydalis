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
  , ProgressKind(..)
  , remainingTime
  ) where

import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified Formatting.ShortFormatters as F
import qualified Formatting.Time            as FT

import           Handler.Utils
import           Import
import           Repository
import           Repository.State

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
repoStatusToScanStyle RepoFinished {}  = "border-success"
repoStatusToScanStyle RepoError {}     = "border-danger text-danger"

repoStatusToRenderStyle :: RepoStatus -> Text
repoStatusToRenderStyle RepoEmpty        = "border-warning"
repoStatusToRenderStyle RepoStarting     = "border-warning"
repoStatusToRenderStyle RepoScanning {}  = "border-warning"
repoStatusToRenderStyle RepoRendering {} = "border-info"
repoStatusToRenderStyle RepoCleaning {}  = "border-success"
repoStatusToRenderStyle RepoFinished {}  = "border-success"
repoStatusToRenderStyle RepoError {}     = "border-danger text-danger"

repoStatusToCleanStyle :: RepoStatus -> Text
repoStatusToCleanStyle RepoEmpty        = "border-warning"
repoStatusToCleanStyle RepoStarting     = "border-warning"
repoStatusToCleanStyle RepoScanning {}  = "border-warning"
repoStatusToCleanStyle RepoRendering {} = "border-warning"
repoStatusToCleanStyle RepoCleaning {}  = "border-info"
repoStatusToCleanStyle RepoFinished {}  = "border-success"
repoStatusToCleanStyle RepoError {}     = "border-danger text-danger"

repoInProgress :: ZonedTime -> Text -> WorkStart -> Widget
repoInProgress now work WorkStart{..} =
  toWidget [hamlet|
            <p .card-text>
               #{work} in progress for <abbr title="Since #{showLocalTimeSeconds wsStart}">#{relTime False (diffZ now wsStart)}</abbr>.
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

-- | Items left to process; never negative, as goals counted in advance
-- (e.g. of the files to scan) can be off.
remainingItems :: ProgressKind -> Progress -> Int
remainingItems kind p = max 0 $ pgGoal p - countedItems kind p

-- | How a phase's progress is described: each phrase is given in the
-- singular and the plural (also used for none), as it follows a count.
data PhaseLabels = PhaseLabels
  { plWork      :: !Text            -- ^ The phase's name (e.g. "Scanning").
  , plKind      :: !ProgressKind
  , plUnit      :: !(Text, Text)    -- ^ What it processes (e.g. "file").
  , plNoop      :: !(Text, Text)    -- ^ The items with nothing to do.
  , plDone      :: !(Text, Text)    -- ^ The items processed.
  , plErrors    :: !(Text, Text)    -- ^ The items that failed.
  , plRemaining :: !(Text, Text)    -- ^ The items left.
  }

scanLabels :: PhaseLabels
scanLabels = PhaseLabels
  { plWork      = scanning
  , plKind      = InvestigateAll
  , plUnit      = ("file", "files")
  -- Files with an up to date metadata cache, and files that are not
  -- pictures, need no reading.
  , plNoop      = ("file needed no reading", "files needed no reading")
  , plDone      = ("file was read", "files were read")
  , plErrors    = ("file had issues", "files had issues")
  , plRemaining = ("file left to scan", "files left to scan")
  }

renderLabels :: PhaseLabels
renderLabels = PhaseLabels
  { plWork      = rendering
  , plKind      = WorkOnly
  , plUnit      = ("preview", "previews")
  , plNoop      = ("preview was already up to date", "previews were already up to date")
  , plDone      = ("preview was built", "previews were built")
  , plErrors    = ("preview failed", "previews failed")
  , plRemaining = ("preview left to build", "previews left to build")
  }

cleanLabels :: PhaseLabels
cleanLabels = PhaseLabels
  { plWork      = cleaning
  , plKind      = InvestigateAll
  , plUnit      = ("file", "files")
  , plNoop      = ( "file belongs to an image in the library"
                  , "files belong to images in the library")
  , plDone      = ("obsolete file was deleted", "obsolete files were deleted")
  , plErrors    = ("file failed to delete", "files failed to delete")
  , plRemaining = ("file left to check", "files left to check")
  }

-- | The singular or the plural of a phrase, for a count.
forCount :: Int -> (Text, Text) -> Text
forCount 1 (one, _) = one
forCount _ (_, others) = others

-- | A count and its phrase (e.g. "no files left to scan").
countPhrase :: Int -> (Text, Text) -> Text
countPhrase n phrase = swissNumOrNone n <> " " <> forCount n phrase

progressDetails :: PhaseLabels -> Progress -> Widget
progressDetails PhaseLabels{..} counter =
  toWidget [hamlet|
                <ul>
                  <li>#{countPhrase (pgNoop counter) plNoop}.
                  <li>#{countPhrase (pgDone counter) plDone}.
                  <li>#{swissNumOrNone errors} <a href=@{StatusErrorsR}>#{forCount errors plErrors}</a>.
                  <li>#{countPhrase (remainingItems plKind counter) plRemaining}.
                  |]
  where errors = pgNumErrors counter

progressThroughput :: PhaseLabels -> Progress -> NominalDiffTime -> Widget
progressThroughput PhaseLabels{plKind = InvestigateAll, plUnit = (_, units)} counter delta =
  toWidget [hamlet|
            <p .card-text>
              Throughput: #{showThroughput $ throughput (pgTotal counter) delta} #{units}/s overall,
              #{showThroughput $ throughput (pgWork counter) delta} #{units}/s for actual work.
              |]
progressThroughput PhaseLabels{plKind = WorkOnly, plUnit = (_, units)} counter delta =
  toWidget [hamlet|
            <p .card-text>
              Throughput: #{showThroughput $ throughput (pgWork counter) delta} #{units}/s.
              |]

workInProgress :: ZonedTime -> PhaseLabels -> Progress -> WorkStart -> Widget
workInProgress now labels counter@Progress{..} WorkStart{..} =
  [whamlet|
          <div .card-body>
            <p .card-text>
               #{work} progress: #{swissNum $ countedItems kind counter}/#{swissNum pgGoal} #{forCount pgGoal (plUnit labels)}:
            ^{progressDetails labels counter}
            <p .card-text>
               #{work} in progress for <abbr title="Since #{showLocalTimeSeconds wsStart}">#{relTime False delta}</abbr>.
               ETA: #
               $maybe at <- eta
                 <abbr title="At #{showLocalTimeSeconds at}">#{relTime True remaining}</abbr>.
               $nothing
                 #{relTime True remaining}.
            ^{progressThroughput labels counter delta}
            ^{percentsBar kind counter}
               |]
  where work = plWork labels
        kind = plKind labels
        remaining = remainingTime kind counter . realToFrac $ diffZ now wsStart
        delta = diffZ now wsStart
        -- The estimated end time; none while the estimate is unknown
        -- (NaN) or infinite, e.g. at the start.
        eta
          | isNaN remaining || isInfinite remaining = Nothing
          | otherwise = Just . utcToZonedTime (zonedTimeZone now)
                        . addUTCTime (realToFrac remaining) $ zonedTimeToUTC now

-- | The estimated time left for work in progress, in seconds, from its
-- progress and the time elapsed: NaN while unknown (e.g. nothing done
-- yet), infinite at the start of work with a known goal, and never
-- negative, as goals counted in advance (e.g. of the files to scan)
-- can be exceeded.
remainingTime :: ProgressKind -> Progress -> Double -> Double
remainingTime kind counter@Progress{..} elapsed =
  -- In this order, as max keeps its first argument when they don't
  -- compare: NaN stays unknown.
  max remaining 0
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
        remaining = elapsed * multiplier - elapsed

-- | Whether the rendering is still in its first pass, checking which
-- previews need rendering (see 'forceBuildThumbCaches'); its own
-- progress only gets a goal once this is done.
renderChecking :: Progress -> Bool
renderChecking checked = pgDone checked < pgGoal checked

-- | The first pass of the rendering, with the images checked so far.
renderCheckInProgress :: ZonedTime -> Progress -> WorkStart -> Widget
renderCheckInProgress now checked WorkStart{..} =
  [whamlet|
          <div .card-body>
            <p .card-text>
              Checking which previews need rendering: #
              #{swissNum $ pgDone checked}/#{swissNum $ pgGoal checked} images.
            <p .card-text>
              #{rendering} in progress for <abbr title="Since #{showLocalTimeSeconds wsStart}">#{relTime False (diffZ now wsStart)}</abbr>.
            ^{percentsBar WorkOnly checked}
               |]

workResults :: ZonedTime -> WorkResults -> PhaseLabels -> Widget
workResults now WorkResults{..} labels =
  [whamlet|
          <div .card-body>
            <p .card-text>
              #{work} finished, #{swissNum processed} #{forCount processed (plUnit labels)} processed:
                 ^{progressDetails labels wrDone}
            <p .card-text>
              #{work} started <abbr title="#{showLocalTimeSeconds wrStart}">#{relTime True (diffZ wrStart now)}</abbr>
              and took <abbr title="Ended at #{showLocalTimeSeconds wrEnd}">#{relTime False delta}</abbr>.
            ^{progressThroughput labels wrDone delta}
            ^{percentsBar kind wrDone}
              |]
  where delta = diffZ wrEnd wrStart
        work = plWork labels
        kind = plKind labels
        processed = countedItems kind wrDone

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
  | pgGoal <= 0 && pgNoop > 0 = (0, 100, 0, 0)
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
      ^{pgBar pN "bg-success" "Nothing to do"}
      ^{pgBar pE "bg-danger" "Failed"}
      ^{pgBar pD "bg-info progress-bar-striped" "Done"}
      ^{pgBar pR "bg-body-tertiary text-secondary" "Left to do"}
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
  -- Read together, so that the end of the rendering's first pass is
  -- seen at once (see 'forceBuildThumbCaches').
  (renderProgress, renderCheckProgress) <- liftIO . atomically $
    (,) <$> readTVar (ctxRenderProgress ctx)
        <*> readTVar (ctxRenderCheckProgress ctx)
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
