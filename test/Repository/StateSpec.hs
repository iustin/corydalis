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

module Repository.StateSpec (spec) where

import qualified Data.Map         as Map
import qualified Data.Text.Short  as TS
import           System.IO.Unsafe (unsafePerformIO)

import           Repository
import           Repository.Scan
import           Repository.State
import           TestImport

spec :: Spec
spec = parallel $ do
  withContext $ do
    describe "search cache" $ do
      it "caches a search result" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
        getSearchResults ctx (const m1) [] `shouldReturn` m1
        getSearchResults ctx (error "Failed to cache") [] `shouldReturn` m1
      it "evaluates the results before caching them" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
            unevaluated = (Map.singleton ("a", (Nothing, "b")) (error "deep"),
                           Map.empty)
        getSearchResults ctx (const unevaluated) [] `shouldThrow` anyErrorCall
        -- Nothing was cached.
        getSearchResults ctx (const m1) [] `shouldReturn` m1
      it "keeps the search cache when only a scan's status changes" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
        serial <- atomically $ newRepo (ctxRepo ctx)
        getSearchResults ctx (const m1) [] `shouldReturn` m1
        -- A failing scan only changes the status, not the data.
        _ <- runScanAction ctx serial (throwString "scan exploded")
        getSearchResults ctx (error "Not cached") [] `shouldReturn` m1
      -- A search listing the repository's folders, which publishes new
      -- data (one more folder) while it runs, the given number of times:
      -- as a scan would, between the search reading the repository and
      -- storing its results. The search is pure, so the publishing is
      -- done when its results are evaluated (with unsafePerformIO). The
      -- number of searches run is counted.
      let racingSearch ctx publishes = do
            runs <- newIORef (0 :: Int)
            let image = simpleRawImage (ctxConfig ctx)
                listed repo = (Map.empty, image <$ repoDirs repo)
                search repo = unsafePerformIO $ do
                  n <- atomicModifyIORef' runs (\k -> (k + 1, k + 1))
                  when (n <= publishes) $ do
                    let name = TS.pack ("new" ++ show n)
                        dirs = Map.insert name (createTestPicDir name) (repoDirs repo)
                    updateRepo ctx repo { repoDirs = dirs } >>= (`shouldBe` True)
                  return $ listed repo
            return (search, readIORef runs)
          folderNames = Map.keys . snd
      it "doesn't cache results computed from data replaced meanwhile" $ \ctx -> do
        (search, runs) <- racingSearch ctx 1
        -- Searched again, on the new data.
        results <- getSearchResults ctx search []
        folderNames results `shouldBe` ["new1"]
        runs `shouldReturn` 2
        -- And these results were cached.
        getSearchResults ctx (error "Not cached") [] `shouldReturn` results
      it "returns uncached results if the data is replaced again" $ \ctx -> do
        (search, runs) <- racingSearch ctx 2
        results <- getSearchResults ctx search []
        -- From the second search, on the data the first one published.
        folderNames results `shouldBe` ["new1"]
        runs `shouldReturn` 2
        -- Not cached: searched again, on the current data.
        again <- getSearchResults ctx search []
        folderNames again `shouldBe` ["new1", "new2"]
        runs `shouldReturn` 3
      it "uses the results another search cached on the new data" $ \ctx -> do
        runs <- newIORef (0 :: Int)
        let image = simpleRawImage (ctxConfig ctx)
            fresh = (Map.empty, Map.singleton "fresh" image)
            -- As in racingSearch, publishing new data while it runs;
            -- then another search caches its results on that data.
            search repo = unsafePerformIO $ do
              modifyIORef' runs (+ 1)
              let dirs = Map.insert "new1" (createTestPicDir "new1") (repoDirs repo)
              updateRepo ctx repo { repoDirs = dirs } >>= (`shouldBe` True)
              _ <- getSearchResults ctx (const fresh) []
              return (Map.empty, image <$ repoDirs repo)
        results <- getSearchResults ctx search []
        folderNames results `shouldBe` ["fresh"]
        readIORef runs `shouldReturn` 1
      it "flushes the search cache on rescan" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
            m2 = (Map.empty, Map.empty)
        launchScanFileSystem ctx
        _ <- waitForScan ctx
        getSearchResults ctx (const m1) [] `shouldReturn` m1
        getSearchResults ctx (const m2) [] `shouldReturn` m1
        launchScanFileSystem ctx
        _ <- waitForScan ctx
        getSearchResults ctx (const m2) [] `shouldReturn` m2
