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

module Handler.WidgetsSpec (spec) where

import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Yesod.Core

import           Handler.Widgets
import           TestImport

spec :: Spec
spec = parallel $ withApp $
  describe "imageFlagActions" $
    forM_ [True, False] $ \flag -> do
      let render = runHandler $ do
            urlRender <- getUrlRender
            page <- widgetToPageContent $ imageFlagActions "a" "b" flag
            body <- withUrlRenderer $ pageBody page
            return (urlRender $ ImageFlagR "a" "b", toStrict $ renderHtml body)
      it ("points the buttons to the flag URL, flag=" ++ show flag) $ do
        (url, html) <- render
        liftIO $ html `shouldSatisfy`
          isInfixOf ("data-flag-url=\"" <> url <> "\"")
      it ("has the right buttons, flag=" ++ show flag) $ do
        (_, html) <- render
        liftIO $ do
          html `shouldSatisfy` isInfixOf "data-flag=\"false\""
          isInfixOf "data-flag=\"true\"" html `shouldBe` flag
      it ("is not a form, flag=" ++ show flag) $ do
        (_, html) <- render
        liftIO $ do
          html `shouldNotSatisfy` isInfixOf "<form"
          html `shouldNotSatisfy` isInfixOf "_method"
