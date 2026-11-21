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

module Handler.SearchSpec (spec) where

import qualified Data.Map        as Map
import qualified Data.Text       as Text
import           Formatting
import           Handler.Cookies
import           Handler.Search   (skippedMessage)
import           Indexer         (Atom (..), StrOp (..),
                                  maxQuickSearchLength)
import           Pics            (getSearchResults, launchScanFileSystem,
                                  waitForScan)
import           TestImport

checkRedirAndHeader :: YesodExample App ()
checkRedirAndHeader = do
  checkRedirect
  statusIs 200
  htmlAllContain "h1" "Listing folders"

spec :: Spec
spec = parallel $ withApp $ do
  describe "SearchYears" $ do
    it "loads the folders no year page and checks it looks right" $ do
      login
      get SearchFoldersNoYearR
      checkRedirAndHeader
    it "loads empty folder=1900 page and checks it looks right" $ do
      login
      -- Force mode so that we dont' rely on defaults.
      testSetCookie $ lastViewCookie False ViewFoldersList
      get $ SearchFoldersByYearR "1900"
      checkRedirAndHeader
      htmlAnyContain "div.card-header" "Nothing found"
      htmlAnyContain "div.card-body" "doesn't match any folders"
  describe "QuickSearch" $ do
    it "checks for missing search parameter" $ do
      login
      checkRouteIs QuickSearchR 400
    it "checks for invalid search parameter" $ do
      login
      get (QuickSearchR, [("q", "")])
      statusIs 400
    it "rejects a too long search" $ do
      login
      get (QuickSearchR, [("q", Text.replicate (maxQuickSearchLength + 1) "x")])
      statusIs 400
    it "lists at most five skipped filters" $ do
      let atoms n = [ Country (OpEqual (fromString ('c' : show i))) | i <- [1..n :: Int] ]
          five = skippedMessage (atoms 5)
          seven = skippedMessage (atoms 7)
      (liftIO :: IO () -> YesodExample App ()) $ do
        five `shouldSatisfy` ("c5" `isInfixOf`)
        five `shouldSatisfy` (not . ("more" `isInfixOf`))
        seven `shouldSatisfy` ("c5" `isInfixOf`)
        seven `shouldSatisfy` (not . ("c6" `isInfixOf`))
        seven `shouldSatisfy` ("and 2 more." `isSuffixOf`)
    it "checks for fruitless search" $ do
      login
      get (QuickSearchR, [("q", "1900")])
      checkRedirect
  describe "Search" $ do
    forM_ [ListFoldersR, ListImagesR, BrowseImagesR 0] $ \route -> do
      it (formatToString ("checks redirect for route " % shown) route) $ do
        login
        checkRoute route
        get SearchR
        --location <- getLocation
        --liftIO $ print loc
        --liftIO $ location `shouldBe` Right route
        followRedirectOK
        liftIO $ pendingWith "route parsing is broken"

    forM_ [ListFoldersR, ListImagesR, BrowseImagesR 0] $ \route ->
      it (formatToString ("refuses missing values of folders for route " % shown) route) $ do
        login
        get (route, [("no-folder" :: Text, "")])
        statusIs 400

  describe "SearchView" $ do
    it "Reports no images on empty repository" $ do
      login
      get SearchViewR
      htmlAnyContain "div.card-header" "Nothing found"
    it "Redirects to first image when there is one" $ do
      app <- getTestYesod
      let ctx = appContext app
      -- force scan filesystem
      liftIO $ launchScanFileSystem ctx
      _ <- liftIO $ waitForScan ctx
      -- populate cache
      let image = simpleRawImage (ctxConfig ctx)
          results = (Map.singleton ("a", (Nothing, "b")) image, Map.singleton "a" image)
          allparams = ("all", "0")
      liftIO $ getSearchResults ctx results  [allparams] `shouldReturn` results
      login
      request $ do
        uncurry addGetParam allparams
        setMethod "GET"
        setUrl SearchViewR
      checkRedirect
