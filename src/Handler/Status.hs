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

repoInProgress :: ZonedTime -> Text -> WorkStart -> Widget
repoInProgress now work WorkStart{..} =
  toWidget [hamlet|
          <div .card-body .text-info>
            <p .card-text>
               #{work} in progress for <abbr title="Since #{show wsStart}">#{relTime False (diffZ now wsStart)}</abbr>.
               |]

workInProgress :: ZonedTime -> Text -> Progress -> WorkStart -> Widget
workInProgress now work counter WorkStart{..} =
  toWidget [hamlet|
            <p .card-text>
               #{work} progress: #{swissNum (pgTotal counter)}/#{swissNum wsGoal}.
            <p .card-text>
              $if pgErrors counter > 0
                A total of #{pgErrors counter} errors have occured so far.
              $else
                No errors found (yet).
            <p .card-text>
               #{work} in progress for <abbr title="Since #{show wsStart}">#{relTime False (diffZ now wsStart)}</abbr>.
               ETA: #{relTime True remaining}.
               |]
  where multiplier = (fromIntegral wsGoal::Double) / fromIntegral (pgTotal counter)
        elapsed = realToFrac $ diffZ now wsStart
        totaltime = elapsed * multiplier
        remaining = totaltime - elapsed
        -- TODO: add actual ETA once upgrading to newer time library [easy] [dependency].

workResults :: ZonedTime -> WorkResults -> Text -> Text -> Widget
workResults now WorkResults{..} work item =
  toWidget [hamlet|
          <div .card-body>
            <p .card-text>
              #{work} finished, #{swissNum wrDone} #{item} processed.
            $if wrErrors > 0
             <p .card-text>
               A total of #{wrErrors} errors have occured.
            <p .card-text>
              #{work} started <abbr title="#{show wrStart}">#{relTime True (diffZ wrStart now)}</abbr>
              and took <abbr title="Ended at #{show wrEnd}">#{relTime False delta}</abbr>.
            <p .card-text>
              #{work} throughput: #{showThroughput $ throughput wrDone delta} #{item}/s.
              |]
  where delta = diffZ wrEnd wrStart

renderIdle :: Widget
renderIdle =
  toWidget [hamlet|
          <div .card-body .text-warning>
            Rendering has not started yet.
            |]

scanFailed :: Widget
scanFailed =
  toWidget [hamlet|
          <div .card-body .text-danger>
            Repository scanning failed.
            |]

getStatusR :: Handler Html
getStatusR = do
  repo <- getPics
  let repoState = repoStatus repo
  scanProgress <- liftIO getProgress
  renderCur <- liftIO getRenderProgress
  let renderTotal = case repoState of
        RepoRendering _ ws -> Just $ wsGoal ws
        RepoFinished _ wr  -> Just $ wrGoal wr
        _                  -> Nothing
      renderDone = pgTotal renderCur
      renderPercent = do
        rt <- renderTotal
        if rt > 0
          then Just (truncate (fromIntegral renderDone * 100 / (fromIntegral rt::Double)))
          else Nothing::Maybe Int
      (overall_perc, overall_text, overall_role, overall_strip) = case repoState of
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
