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
import           Handler.Browse   (getBrowseImagesR)
import           Handler.List     (getListFoldersR, getListImagesR)
import           Handler.Search   (getQuickSearchR, getSearchR,
                                   getSearchViewR, skippedMessage,
                                   getBestHandler)
import           Handler.View     (getRandomImageInfoR)
import           Indexer         (Atom (..), StrOp (..),
                                  maxQuickSearchLength)
import           Repository.Scan  (launchScanFileSystem, waitForScan)
import           Repository.State (getSearchResults)
import           TestImport

checkRedirAndHeader :: YesodExample App ()
checkRedirAndHeader = do
  checkRedirect
  statusIs 200
  htmlAllContain "h1" "Listing folders"

spec :: Spec
spec = do
  bestHandlerSpec
  handlerSpec

bestHandlerSpec :: Spec
bestHandlerSpec = describe "getBestHandler" $ do
  it "shows folders as a list" $
    forM_ (Nothing : map Just [minBound .. maxBound]) $ \mode ->
      getBestHandler mode False `shouldBe` ListFoldersR
  it "shows images in the last view mode, as a grid by default" $ do
    getBestHandler Nothing True `shouldBe` BrowseImagesR 0
    getBestHandler (Just ViewImagesGrid) True `shouldBe` BrowseImagesR 0
    getBestHandler (Just ViewImagesList) True `shouldBe` ListImagesR
    getBestHandler (Just ViewSingleImage) True `shouldBe` SearchViewR
    getBestHandler (Just ViewFoldersList) True `shouldBe` ListFoldersR

handlerSpec :: Spec
handlerSpec = parallel $ withApp $ do
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
  describe "problems" $ do
    it "searches by them for logged in users" $ do
      injectProblemImage
      login
      get (ListImagesR, [("problem", "~bad format")])
      statusIs 200
      htmlAllContain "h1" "Listing images"
      bodyNotContains "Nothing found"
    it "refuses searches using them to anonymous users" $ do
      injectProblemImage
      forM_ [ [("problem", "~bad format")]
            , [("no-problem", "")]
            , [("country", "Spain"), ("problem", "~bad"), ("not", ""), ("or", "")]
            ] $ \params -> do
        r <- runAnonymousHandler params getListImagesR
        liftIO $ void r `shouldSatisfy` isPermissionDenied "problem"
      page <- anonymousPage [("country", "Spain")] getListImagesR
      liftIO $ page `shouldSatisfy` ("Listing images" `isInfixOf`)
    it "refuses them in all searches for anonymous users" $ do
      injectProblemImage
      let params = [("problem", "~bad format")]
      results <- sequence
        [ void <$> runAnonymousHandler params getListFoldersR
        , void <$> runAnonymousHandler params getListImagesR
        , void <$> runAnonymousHandler params (getBrowseImagesR 0)
        , void <$> runAnonymousHandler params getSearchR
        , void <$> runAnonymousHandler params getSearchViewR
        , void <$> runAnonymousHandler params getRandomImageInfoR
        ]
      liftIO $ forM_ results (`shouldSatisfy` isPermissionDenied "problem")
    it "quick searches by them for logged in users" $ do
      injectProblemImage
      login
      get (QuickSearchR, [("q", "format")])
      statusIs 303
      location <- responseLocation
      liftIO $ location `shouldSatisfy` maybe False ("problem=" `isInfixOf`)
      -- And it finds the picture.
      followRedirectOK
      bodyNotContains "Nothing found"
    it "doesn't quick search by them for anonymous users" $ do
      injectProblemImage
      forM_ ["format", "problem:format"] $ \q -> do
        r <- runAnonymousHandler [("q", q)] getQuickSearchR
        liftIO $ do
          redirectTarget r `shouldSatisfy`
            maybe False (not . ("problem=" `isInfixOf`))
          -- The other symbols are still searched.
          redirectTarget r `shouldSatisfy` maybe False ("folder=" `isInfixOf`)
      -- And the allowed ones as prefixes.
      r <- runAnonymousHandler [("q", "country:Spain")] getQuickSearchR
      liftIO $ redirectTarget r `shouldSatisfy`
        maybe False (\l -> "country=" `isInfixOf` l && not ("folder=" `isInfixOf` l))

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
      liftIO $ getSearchResults ctx (const results) [allparams] `shouldReturn` results
      login
      request $ do
        uncurry addGetParam allparams
        setMethod "GET"
        setUrl SearchViewR
      checkRedirect
