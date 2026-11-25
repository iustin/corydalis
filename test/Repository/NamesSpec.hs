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

import           Repository.Names
import           TestImport

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
      it "expands ranges only from the file name" $ \config' -> do
        let config = withUnanchoredNameRegexes config'
        expandRangeFile config "sub/img_1-2" `shouldBe` ["sub/img_1", "sub/img_2"]
        expandRangeFile config "a/b/img_1-2" `shouldBe` ["a/b/img_1", "a/b/img_2"]
        expandRangeFile config "img_1-2/b" `shouldBe` []
      it "expands padded range names" $ \config -> do
        expandRangeFile config "img_01-03" `shouldBe` ["img_01", "img_02", "img_03"]
        expandRangeFile config "img_1-3" `shouldBe` ["img_1", "img_2", "img_3"]
        expandRangeFile config "nope" `shouldBe` []
      it "rejects inverted or huge ranges" $ \config -> do
        expandRangeFile config "img_9-1" `shouldBe` []
        expandRangeFile config "img_1-1025" `shouldBe` []
        expandRangeFile config "img_3982-1773958749527" `shouldBe` []
        length (expandRangeFile config "img_1-1024") `shouldBe` maxRangeExpansion
