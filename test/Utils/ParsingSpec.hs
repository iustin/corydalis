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

module Utils.ParsingSpec (spec) where

import           Data.Char       (isDigit)
import           Formatting
import           Test.QuickCheck

import           TestImport
import           Utils.Parsing

-- Generate a single non-digit alphabetic character
arbitraryNonDigitChar :: Gen Char
arbitraryNonDigitChar = arbitraryPrintableChar `suchThat` (not . isDigit)

-- Generate an arbitrary alphabetic string with no digits
arbitraryNonDigitString :: Gen String
arbitraryNonDigitString = listOf arbitraryNonDigitChar

spec :: Spec
spec = parallel $ do
  describe "checks parseDecimal" $ do
    prop "parses numbers correctly" $
      \num -> parseDecimal (sformat int (num::Int)) `shouldBe` Right num
    prop "rejects leftover chars" $
      \num -> (parseDecimal (sformat (int % char) (num::Int) 'a')::Either Text Int) `shouldBeLeftWithMessage` "leftover"
    prop "rejects non-numbers" $
      forAll arbitraryNonDigitString $
        \alphastr -> (parseDecimal (sformat string alphastr)::Either Text Int) `shouldBeLeftWithMessage` "does not start with a digit"

  describe "checks parseReal" $ do
    -- Below, only parsing positive numbers due to
    -- https://github.com/AJChapman/formatting/issues/88
    prop "parses numbers correctly" $
      \num ->
        let positiveNum = abs num
            parsed = parseReal (sformat float (positiveNum::Double))
        in case parsed of
             Right parsedNum -> parsedNum `shouldSatisfy` (\p -> abs (p - positiveNum) < 1e-10)
             Left err -> expectationFailure $ "Failed to parse: " ++ show err
    prop "rejects leftover chars" $
      \num ->
        let positiveNum = abs num
        in (parseReal (sformat (float % char) (positiveNum::Double) 'a')::Either Text Double) `shouldBeLeftWithMessage` "leftover"
    prop "rejects non-numbers" $
      forAll arbitraryNonDigitString $
        \alphastr -> (parseReal (sformat string alphastr)::Either Text Double) `shouldBeLeftWithMessage` "does not start with a digit"
