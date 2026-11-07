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

import qualified Data.Text                     as Text
import           Text.Blaze.Html.Renderer.Text (renderHtml)
import           Yesod.Core

import           Handler.Widgets
import           TestImport

spec :: Spec
spec = parallel $ withApp $
  describe "imageFlagActions" $
    forM_ [True, False] $ \flag ->
      it ("includes the CSRF token in the form, flag=" ++ show flag) $ do
        (token, html) <- runHandler $ do
          mtok <- reqToken <$> getRequest
          page <- widgetToPageContent $ imageFlagActions "a" "b" flag
          body <- withUrlRenderer $ pageBody page
          return (mtok, toStrict $ renderHtml body)
        case token of
          Nothing -> liftIO $ expectationFailure "No token in the request"
          Just tok -> liftIO $ do
            html `shouldSatisfy`
              isInfixOf ("name=\"_token\" value=\"" <> tok <> "\"")
            -- methodOverridePost only honours _method as the first field,
            -- and browsers send the fields in document order.
            fst (Text.breakOn "name=\"_token\"" html) `shouldSatisfy`
              isInfixOf "name=\"_method\""
