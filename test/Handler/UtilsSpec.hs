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

import           Data.Aeson    (encode)
import           Data.Default  (def)
import qualified Data.ByteString.Lazy as BSL

import           Handler.Utils (GraphData (..), PlotText (..), escapePlotText,
                                formatPeriod, plotText)

spec :: Spec
spec = do
  formatPeriodSpec
  plotTextSpec

plotTextSpec :: Spec
plotTextSpec = describe "chart text" $ do
  it "escapes the characters Plotly reads as markup" $ do
    escapePlotText "<a href=\"https://evil.example\">Canon</a>"
      `shouldBe` "&lt;a href=\"https://evil.example\"&gt;Canon&lt;/a&gt;"
    -- Ampersands first, so that entities show as written.
    escapePlotText "A&amp;B" `shouldBe` "A&amp;amp;B"
  it "keeps quotes, which Plotly wouldn't decode as entities" $
    escapePlotText "EF 70-200mm \"II\" 'L'" `shouldBe` "EF 70-200mm \"II\" 'L'"
  it "is escaped in the charts' data, but not the trusted parts" $ do
    let series = def { gdName = plotText "<b>Nikon</b>"
                     , gdX = Just [plotText "<i>Z</i>"]
                     , gdY = Just [1 :: Int]
                     , gdText = Just [PlotText "a&b"]
                     , gdExtra = [("hovertemplate", "%{label}<extra></extra>")]
                     } :: GraphData PlotText Int Int
        json = BSL.toStrict (encode series)
    json `shouldSatisfy` ("&lt;b&gt;Nikon&lt;/b&gt;" `isInfixOf`)
    json `shouldSatisfy` ("&lt;i&gt;Z&lt;/i&gt;" `isInfixOf`)
    json `shouldSatisfy` ("a&amp;b" `isInfixOf`)
    json `shouldSatisfy` ("%{label}<extra></extra>" `isInfixOf`)
    json `shouldNotSatisfy` ("<b>" `isInfixOf`)

formatPeriodSpec :: Spec
formatPeriodSpec = describe "formatPeriod" $ do
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
