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

module Handler.UtilsSpec (spec) where

import           TestImport

import           Handler.Utils (formatPeriod)

spec :: Spec
spec = describe "formatPeriod" $ do
  it "counts both days" $
    formatPeriod (fromGregorian 2024 3 5) (fromGregorian 2024 3 5)
      `shouldBe` "1 day"
  it "uses weeks and days" $
    formatPeriod (fromGregorian 2024 3 1) (fromGregorian 2024 3 10)
      `shouldBe` "1 week and 3 days"
  it "uses calendar months" $ do
    -- February 2024 has 29 days, March 31; both are a month.
    formatPeriod (fromGregorian 2024 2 1) (fromGregorian 2024 2 29)
      `shouldBe` "1 month"
    formatPeriod (fromGregorian 2024 3 1) (fromGregorian 2024 3 31)
      `shouldBe` "1 month"
  it "uses calendar years, and keeps the two largest units" $ do
    formatPeriod (fromGregorian 2023 1 1) (fromGregorian 2023 12 31)
      `shouldBe` "1 year"
    formatPeriod (fromGregorian 2022 1 1) (fromGregorian 2024 3 20)
      `shouldBe` "2 years and 2 months"
