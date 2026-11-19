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

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.StatusSpec (spec) where

import           TestImport

import           Data.Default        (def)
import           Data.Time.LocalTime (ZonedTime, getZonedTime)

import           Pics                (RepoStatus (..), Repository (..))

-- | Sets the state of the repository, as shown by the status page.
--
-- The test app has already finished its initial scan (see
-- 'openTempApp'), so nothing else changes the state afterwards.
setRepoStatus :: (ZonedTime -> RepoStatus) -> YesodExample App ()
setRepoStatus mkStatus = do
  ctx <- appContext <$> getTestYesod
  liftIO $ do
    now <- getZonedTime
    atomically . modifyTVar' (ctxRepo ctx) $ \repo ->
      repo { repoStatus = mkStatus now }

-- | Finished work, started and ended at the given time.
done :: ZonedTime -> WorkResults
done now = WorkResults { wrStart = now, wrEnd = now, wrDone = def }

-- | Checks the status page: its texts, and how many of the three work
-- cards (scan, render, clean) are shown as successfully finished.
checkStatusPage :: [Text] -> Int -> YesodExample App ()
checkStatusPage texts finished = do
  login
  checkRoute StatusR
  htmlAllContain "h1" "Corydalis"
  forM_ texts $ bodyContains . unpack
  htmlCount ".card.border-success" finished

spec :: Spec
spec = parallel $ withApp $ describe "the status page" $ do
  it "shows a finished scan" $
    -- The state left by the initial scan.
    checkStatusPage ["all done", "All work completed."] 3
  it "shows a starting scan" $ do
    setRepoStatus (const RepoStarting)
    checkStatusPage [ "preparing scan", "Preparing to scan the repository."
                    , "Scanning has not started yet."
                    , "Rendering has not started yet."
                    , "Cleaning has not started yet."
                    ] 0
  it "shows a scan in progress" $ do
    setRepoStatus $ \now -> RepoScanning (WorkStart now)
    checkStatusPage [ "scanning filesystem"
                    , "Rendering has not started yet."
                    , "Cleaning has not started yet."
                    ] 0
  it "shows rendering in progress" $ do
    setRepoStatus $ \now -> RepoRendering (done now) (WorkStart now)
    checkStatusPage ["rendering images", "Cleaning has not started yet."] 1
    -- With nothing to render, the estimate is known (now).
    bodyContains "ETA: <abbr title=\"At "
  it "shows no estimated end time when infinite" $ do
    -- Images to render, but none rendered yet: the estimate is infinite.
    setRepoStatus $ \now -> RepoRendering (done now) (WorkStart now)
    ctx <- appContext <$> getTestYesod
    liftIO . atomically $ writeTVar (ctxRenderProgress ctx) def { pgGoal = 5 }
    checkStatusPage ["rendering images", "ETA: "] 1
    bodyNotContains "ETA: <abbr"
  it "shows no estimated end time when unknown" $ do
    -- Nothing scanned yet, so the estimate is unknown.
    setRepoStatus $ \now -> RepoScanning (WorkStart now)
    checkStatusPage ["scanning filesystem", "ETA: "] 0
    bodyNotContains "ETA: <abbr"
  it "shows cleaning in progress" $ do
    setRepoStatus $ \now -> RepoCleaning (done now) (done now) (WorkStart now)
    checkStatusPage ["cleaning the cache"] 2
  it "shows a failed scan" $ do
    setRepoStatus . const $ RepoError "disk on fire"
    checkStatusPage [ "Repository scanning failed", "disk on fire"] 0
