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

module ApplicationSpec (spec) where

import           Application (redactPassword)
import           TestImport

spec :: Spec
spec = describe "redactPassword" $ do
  it "hides the password" $
    redactPassword ("password", "secret") `shouldBe` Just ("password", "(hidden)")
  it "keeps the other parameters" $ do
    redactPassword ("username", "alice") `shouldBe` Just ("username", "alice")
    redactPassword ("_token", "abc") `shouldBe` Just ("_token", "abc")
