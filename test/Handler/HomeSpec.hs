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

checkLoginSuccessful :: YesodExample App ()
checkLoginSuccessful = do
      userEntity <- createUser "foo"
      authenticateAs userEntity

      get HomeR
      statusIs 200

spec :: Spec
spec = withApp $ do
    it "asserts redirect on access to home for anonymous users" $ do
        get HomeR
        statusIs 303

    it "asserts access to home for authenticated users"
      checkLoginSuccessful
    it "loads the index and checks it looks right" $ do
      checkLoginSuccessful
      htmlAllContain "h1" "Corydalis"
      htmlAnyContain "div#main" "Years"
      htmlAnyContain "div#main" "Countries"
      htmlAnyContain "div#main" "Locations"
      htmlAnyContain "div#main" "People"
      htmlAnyContain "div#main" "Keywords"
