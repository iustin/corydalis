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

workInProgress :: ZonedTime -> Text -> WorkStart -> Widget
workInProgress now work WorkStart{..} =
  toWidget [hamlet|
          <div .card-body .text-info>
            <p .card-text>
               #{work} in progress for #{sformat (F.f 2) (diffZ now wsStart)} seconds.
               |]

scanResults :: WorkResults -> Widget
scanResults WorkResults{..} =
  toWidget [hamlet|
          <div .card-body>
            <p .card-text>
              Repository scan finished, #{swissNum wrDone} items scanned.
            <p .card-text>
              Scan started at #{show wrStart} and finished at #{show wrEnd}.
            <p .card-text>
              Scan duration: #{sformat (F.f 2) delta} seconds.
            <p .card-text>
              Scan throughput: #{showThroughput $ throughput wrDone delta} files/s.
              |]
  where delta = diffZ wrEnd wrStart

getStatusR :: Handler Html
getStatusR = do
  pics <- getPics
  config <- getConfig
  let repoState = repoStatus pics
  scanProgress <- liftIO getProgress
  (renderCur, renderTotal) <- liftIO $ getRenderProgress config pics
  let renderPercent = if renderTotal > 0
                      then Just (truncate (fromIntegral renderCur * 100 / (fromIntegral renderTotal::Double)))
                      else Nothing::Maybe Int
  now <- liftIO getZonedTime
  defaultLayout $ do
    setHtmlTitle "status"
    $(widgetFile "status")
