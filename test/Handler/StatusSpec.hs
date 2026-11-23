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

import           Handler.Status      (ProgressKind (..), remainingTime)
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
spec = do
  remainingTimeSpec
  pageSpec

remainingTimeSpec :: Spec
remainingTimeSpec = describe "remainingTime" $ do
  it "estimates from the work done" $
    remainingTime InvestigateAll def { pgGoal = 4, pgDone = 2 } 60 `shouldBe` 60
  it "is never negative, even if the goal is exceeded" $ do
    -- More files examined than counted in advance.
    remainingTime InvestigateAll def { pgGoal = 1, pgNoop = 3 } 60 `shouldBe` 0
    remainingTime WorkOnly def { pgGoal = 1, pgDone = 3 } 60 `shouldBe` 0
  it "is unknown with nothing to go by, and infinite at the start" $ do
    remainingTime InvestigateAll def 60 `shouldSatisfy` isNaN
    remainingTime WorkOnly def { pgGoal = 5 } 60 `shouldSatisfy` isInfinite

pageSpec :: Spec
pageSpec = parallel $ withApp $ describe "the status page" $ do
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
  it "shows the check of what needs rendering, while in progress" $ do
    setRepoStatus $ \now -> RepoRendering (done now) (WorkStart now)
    ctx <- appContext <$> getTestYesod
    liftIO . atomically $
      writeTVar (ctxRenderCheckProgress ctx) def { pgGoal = 10, pgDone = 3 }
    checkStatusPage [ "rendering images"
                    , "Checking which previews need rendering: 3/10 images."
                    ] 1
    bodyNotContains "Rendering progress:"
    -- Once the check is done, the rendering progress shows.
    liftIO . atomically $
      writeTVar (ctxRenderCheckProgress ctx) def { pgGoal = 10, pgDone = 10 }
    checkRoute StatusR
    bodyContains "Rendering progress:"
    bodyNotContains "Checking which previews"
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
  it "never shows a negative number of items left" $ do
    -- More files examined than counted in advance.
    let overcounted now = (done now) { wrDone = def { pgGoal = 1, pgNoop = 3 } }
    setRepoStatus $ \now -> RepoFinished (done now) (done now) (overcounted now)
    checkStatusPage ["no files left to check."] 3
    bodyNotContains "-2 files"
  it "describes the work of each phase in its own words" $ do
    let results now p = (done now) { wrDone = p }
    setRepoStatus $ \now ->
      RepoFinished (results now def { pgGoal = 3, pgNoop = 2, pgDone = 1 })
                   (results now def { pgGoal = 2, pgNoop = 1, pgDone = 2 })
                   (results now def { pgGoal = 4, pgNoop = 3, pgDone = 1 })
    checkStatusPage [ -- The scan.
                      "Scanning finished, 3 files processed:"
                    , "2 files needed no reading."
                    , "1 file was read."
                    , "no files left to scan."
                      -- The rendering.
                    , "Rendering finished, 2 previews processed:"
                    , "1 preview was already up to date."
                    , "2 previews were built."
                    , "no previews left to build."
                      -- The cleaning.
                    , "Cleaning finished, 4 files processed:"
                    , "3 files belong to images in the library."
                    , "1 obsolete file was deleted."
                    , "no files left to check."
                    ] 3
    bodyContains "files failed to delete</a>."
    bodyNotContains "items"
    -- The link around the failures is closed once.
    bodyNotContains "</a>.</a>"
  it "describes work in progress, with failures, in its own words" $ do
    setRepoStatus $ \now -> RepoScanning (WorkStart now)
    ctx <- appContext <$> getTestYesod
    liftIO . atomically . writeTVar (ctxScanProgress ctx) $
      def { pgGoal = 2, pgErrors = [ProgressError "a.jpg" "broken"] }
    checkStatusPage [ "Scanning progress: 1/2 files:"
                    , ">file had issues</a>."
                    , "1 file left to scan."
                    ] 0
  it "shows a failed scan" $ do
    setRepoStatus . const $ RepoError "disk on fire"
    checkStatusPage [ "Repository scanning failed", "disk on fire"] 0
