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

module Handler.ReloadSpec (spec) where

import           Yesod.Core (defaultCsrfParamName)

import           TestImport

spec :: Spec
spec = parallel $ withApp $ do
  it "loads the reload page and checks GET doesn't work" $ do
    login
    checkRouteIs ReloadR 405
  it "loads the reload page and checks redirect message" $ do
    login
    performWithToken "POST" ReloadR
    followRedirectOK
    htmlAllContain "span#corydalis-app-message" "Rescan triggered"
  it "rejects a request without a CSRF token" $ do
    login
    post ReloadR
    statusIs 403
  it "rejects a request with a wrong CSRF token" $ do
    login
    request $ do
      setMethod "POST"
      setUrl ReloadR
      addPostParam defaultCsrfParamName "wrong"
    statusIs 403
  it "accepts the token from the form in the page layout" $ do
    login
    checkRoute HomeR
    request $ do
      setMethod "POST"
      setUrl ReloadR
      addPostParam "unused" "unused"
      addToken_ "form[action$=/reload] "
    followRedirectOK
    htmlAllContain "span#corydalis-app-message" "Rescan triggered"
  it "includes the token in the form on the curate page" $ do
    login
    checkRoute CurateR
    -- One in the page layout, one in the page itself.
    htmlCount "form[action$=/reload] input[name=_token]" 2
