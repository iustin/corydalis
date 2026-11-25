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

module Repository.NamesSpec (spec) where

import           Repository          (ImageRange (..))
import           Repository.Names
import           TestImport

-- | The names in the range of a file name (without extension), if it
-- has one.
rangeNamesOf :: Config -> FilePath -> [ImageName]
rangeNamesOf config = maybe [] rangeNames . snd . parseImageName config

spec :: Spec
spec = parallel $ do
  withConfig $ do
    describe "path regex helpers" $ do
      it "drops copy suffixes" $ \config -> do
        dropCopySuffix config "IMG_1234-1" `shouldBe` "IMG_1234"
        dropCopySuffix config "IMG_1234-Edit" `shouldBe` "IMG_1234"
        dropCopySuffix config "IMG_1234" `shouldBe` "IMG_1234"
      it "drops copy suffixes only from the file name" $ \config' -> do
        let config = withUnanchoredNameRegexes config'
        dropCopySuffix config "sub/IMG_1234-1" `shouldBe` "sub/IMG_1234"
        dropCopySuffix config "2024-01-02-day/b" `shouldBe` "2024-01-02-day/b"
        dropCopySuffix config "a-1/b-Edit" `shouldBe` "a-1/b"
      it "drops copy suffixes only at the end of the name" $ \config ->
        dropCopySuffix config "IMG-20240101_1234" `shouldBe` "IMG-20240101_1234"
      it "reads ranges only from the file name" $ \config' -> do
        let config = withUnanchoredNameRegexes config'
        rangeNamesOf config "sub/img_1-2" `shouldBe` ["sub/img_1", "sub/img_2"]
        rangeNamesOf config "a/b/img_1-2" `shouldBe` ["a/b/img_1", "a/b/img_2"]
        rangeNamesOf config "img_1-2/b" `shouldBe` []
      it "reads padded range names" $ \config -> do
        rangeNamesOf config "img_01-03" `shouldBe` ["img_01", "img_02", "img_03"]
        rangeNamesOf config "img_1-3" `shouldBe` ["img_1", "img_2", "img_3"]
        rangeNamesOf config "nope" `shouldBe` []
      it "rejects inverted or huge ranges" $ \config -> do
        rangeNamesOf config "img_9-1" `shouldBe` []
        rangeNamesOf config "d_05-01" `shouldBe` []
        rangeNamesOf config "img_1-1025" `shouldBe` []
        rangeNamesOf config "img_3982-1773958749527" `shouldBe` []
        length (rangeNamesOf config "img_1-1024") `shouldBe` 1024
      it "rejects numbers too large to read, instead of wrapping them" $ \config -> do
        -- 2^64 + 3, which would wrap around to 3 when read as an Int.
        rangeNamesOf config "img_1-18446744073709551619" `shouldBe` []
        rangeNamesOf config "img_18446744073709551617-18446744073709551619" `shouldBe` []
      it "limits ranges to the configured size" $ \config' -> do
        let config = config' { cfgMaxRangeSize = 3 }
        rangeNamesOf config "img_1-3" `shouldBe` ["img_1", "img_2", "img_3"]
        rangeNamesOf config "img_1-4" `shouldBe` []
      it "reads a wrapped camera counter as a range" $ \config -> do
        rangeNamesOf config "dsc_9998-0002" `shouldBe`
          ["dsc_9998", "dsc_9999", "dsc_0001", "dsc_0002"]
        -- More names than the limit.
        rangeNamesOf config "dsc_1920-0002" `shouldBe` []
        -- Not four digits, or a last number of zero.
        rangeNamesOf config "dsc_998-002" `shouldBe` []
        rangeNamesOf config "dsc_9998-02" `shouldBe` []
        rangeNamesOf config "dsc_998-0002" `shouldBe` []
        rangeNamesOf config "dsc_9998-0000" `shouldBe` []
        -- The limit, at the boundary.
        length (rangeNamesOf config "dsc_9000-0024") `shouldBe` 1024
        rangeNamesOf config "dsc_9000-0025" `shouldBe` []
        fmap rangeSize (snd $ parseImageName config "dsc_9998-0002") `shouldBe` Just 4
      it "decides between a copy and a range by the two numbers" $ \config -> do
        parseImageName config "dsc_1920-2" `shouldBe` ("dsc_1920", Nothing)
        fst (parseImageName config "dsc_1920-1925") `shouldBe` "dsc_1920-1925"
        rangeNamesOf config "dsc_1920-1925" `shouldBe`
          ["dsc_1920", "dsc_1921", "dsc_1922", "dsc_1923", "dsc_1924", "dsc_1925"]
        parseImageName config "x_1-3" `shouldBe`
          ("x_1-3", Just (ImageRange "x_" 1 3 1))
        -- A copy of a composite image.
        parseImageName config "d_01-03-Edit" `shouldBe`
          ("d_01-03", Just (ImageRange "d_" 1 3 2))
