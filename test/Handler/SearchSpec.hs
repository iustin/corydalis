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
import           Formatting
import           Handler.Cookies
import           Pics            (getSearchResults, launchScanFileSystem,
                                  waitForScan)
import           TestImport
import           Types

checkRedirAndHeader :: YesodExample App ()
checkRedirAndHeader = do
  checkRedirect
  statusIs 200
  htmlAllContain "h1" "Listing folders"

spec :: Spec
spec = parallel $ withApp $ do
  -- TODO: add folders by year
  -- TODO: add quick search
  describe "SearchYears" $ do
    it "loads the folders no year page and checks it looks right" $ do
      login
      get SearchFoldersNoYearR
      checkRedirAndHeader
    it "loads empty folder=1900 page and checks it looks right" $ do
      login
      -- Force mode so that we dont' rely on defaults.
      testSetCookie $ lastViewCookie False (ViewFolders PresentationGrid)
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
    it "checks for fruitless search" $ do
      login
      get (QuickSearchR, [("q", "1900")])
      checkRedirect
  describe "Search" $ do
    forM_ [ListFoldersR, ListImagesR, BrowseFoldersR 0, BrowseImagesR 0] $ \route -> do
      it (formatToString ("checks redirect for route " % shown) route) $ do
        login
        checkRoute route
        get SearchR
        --location <- getLocation
        --liftIO $ print loc
        --liftIO $ location `shouldBe` Right route
        followRedirectOK
        liftIO $ pendingWith "route parsing is broken"

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
      let image = simpleImage (ctxConfig ctx)
          results = (Map.singleton ("a", (Nothing, "b")) image, Map.singleton "a" image)
          allparams = ("all", "0")
      liftIO $ getSearchResults ctx results  [allparams] `shouldReturn` results
      login
      request $ do
        uncurry addGetParam allparams
        setMethod "GET"
        setUrl SearchViewR
      checkRedirect
