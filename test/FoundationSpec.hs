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

module FoundationSpec (spec) where

import qualified Data.Map        as M
import           Web.Cookie

import           Handler.Cookies
import           TestImport
import           Types

handlerView :: [(Route App, ViewMode)]
handlerView = [ (BrowseImagesR 0, ViewImages PresentationGrid)
              , (ListImagesR, ViewImages PresentationList)
              ]

checkViewCookie :: Maybe ViewMode -> YesodExample App ()
checkViewCookie expected = request $ do
  cookies <- getRequestCookies
  let actual = do
        cookieB <- M.lookup (encodeUtf8 viewCookieName) cookies
        parseViewMode . decodeUtf8 . setCookieValue $ cookieB
  liftIO $ actual `shouldBe` expected

spec :: Spec
spec = parallel $ withApp $ do
  describe "checks view pages set their cookie" $ do
    forM_ handlerView $ \(route, symbol) ->
      it ("validates route " ++ show route) $ do
      login
      checkRoute route
      checkViewCookie $ Just symbol
  it "checks that a normal handler doesn't reset the view mode" $ do
    login
    forM_ [CurateR, LensStatsR] $ \route -> do
      checkRoute route
      checkViewCookie Nothing
