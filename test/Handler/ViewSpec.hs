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

module Handler.ViewSpec (spec) where

import           Handler.View
import           Pics
import           TestImport
import           Types

import           Data.Aeson

import           Text.Blaze.Svg.Renderer.String

spec :: Spec
spec = parallel $ do
  describe "Internal functionality" $ do
    describe "SVG tests" $ do
      it "generates valid SVG with error message" $ do
        let svg = basicSvg "Test error message"
            rendered = renderSvg svg
        rendered `shouldContain` "Test error message"
        rendered `shouldContain` "<svg"
        rendered `shouldContain` "viewBox=\"0 0 600 800\""

    describe "ImageInfo JSON serialization" $ do
      it "serializes ImageInfo correctly" $ do
        let imgInfo = ImageInfo
              { iiInfoUrl = "/info"
              , iiBytesUrl = "/bytes"
              , iiMovieUrl = Nothing
              , iiViewUrl = "/view"
              , iiFlagUrl = "/flag"
              , iiListUrl = "/list"
              , iiBrowseUrl = "/browse"
              , iiName = ImageName "test.jpg"
              , iiTransform = (0, False, False)
              , iiMatrix = (1.0, 0.0, 0.0, 1.0)
              }
        toJSON imgInfo `shouldNotBe` Null

  describe "handler tests" $ withApp $ do
    describe "checks 404 results" $
      forM_ [ ViewR "foo" "bar"
            , ImageInfoR "foo" "bar"
            , ImageBytesR "foo" "bar"
            , MovieBytesR "foo" "bar"
            , RandomImageInfoR
            ] $ \ route ->
       it ("checks route " ++ show route) $ do
         login
         checkNotFound route
    describe "checks the view page" $ do
      it "loads the view page and checks it looks right" $
        const pending
    describe "returns correct error on non-viewable items" $
      forM_ [("ImageBytesR", ImageBytesR), ("MovieBytesR", MovieBytesR)] $ \(routeName, route) ->
        it ("checks route" ++ routeName) $ do
          app <- getTestYesod
          let ctx = appContext app
          -- first make sure that scan is finished, so that it won't override our artificial image.
          _ <- liftIO $ launchScanFileSystem ctx
          _ <- liftIO $ waitForScan ctx
          let img = simpleUntrackedImage (ctxConfig ctx) "test" "a"
          injectImages [img]
          login
          get $ route "test" (ImageName "a")
          statusIs 200
          bodyContains "Error:"
          bodyContains "<svg"
          bodyContains "Image has no viewable version"
