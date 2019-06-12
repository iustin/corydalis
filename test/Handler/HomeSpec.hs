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

module Handler.HomeSpec (spec) where

import           TestImport

spec :: Spec
spec = parallel $ withApp $ do
  it "asserts redirect on access to home for anonymous users" $
    checkRouteIs HomeR 303

  it "checks that login page works un-authenticated" $ do
    checkRoute $ AuthR LoginR
    htmlAnyContain "div#info" "Hello user!"

  it "checks that login page shows info message" $ do
    get $ AuthR LoginR
    htmlAnyContain "div#info" "Hello user!"

  it "asserts access to home for authenticated users" $ do
    login
    checkRoute HomeR

  it "loads the index and checks it looks right" $ do
    login
    checkRoute HomeR
    htmlAllContain "h1" "Corydalis"
    htmlAnyContain "div#main" "Years"
    htmlAnyContain "div#main" "Countries"
    htmlAnyContain "div#main" "Locations"
    htmlAnyContain "div#main" "People"
    htmlAnyContain "div#main" "Keywords"
