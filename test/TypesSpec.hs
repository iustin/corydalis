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

module TypesSpec (spec) where

import           TestImport
import           Types

import           ClassyPrelude.Yesod
import           Data.Maybe          (fromJust)
import qualified Data.Set
import           Data.Store
import qualified Data.Text           as Text
import           Test.QuickCheck

alphaAndDigits :: Gen Char
alphaAndDigits = elements $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

plainRegexInput :: Gen String
plainRegexInput = listOf1 alphaAndDigits

genRegex :: Gen Regex
genRegex = do
  str <- plainRegexInput
  case mkRegex (Text.pack str) of
    Nothing -> error $ "genRegex: mkRegex failed on input: " ++ str
    Just re -> return re

allViewModes :: [ViewMode]
allViewModes = [ ViewImagesGrid
               , ViewImagesList
               , ViewFoldersList
               ]

spec :: Spec
spec = parallel $ do
  describe "checks view mode encoding/decoding" $ do
    forM_ allViewModes $ \symbol ->
      it ("validates symbol " ++ show symbol) $ do
         let parsed = parseViewMode . decodeUtf8 . formatViewMode $ symbol
         parsed `shouldBe` Just symbol
    it "checks invalid value decoding" $ do
      parseViewMode "foobar" `shouldBe` Nothing
      parseViewMode "" `shouldBe` Nothing
  describe "checks image sizes config parsing" $ withContext $ do
    it "checks duplicates between auto and on-demand" $ \ctx -> do
      let c = ctxConfig ctx
          common = cfgAutoImageSizes c  `intersect` cfgOnDemandSizes c
      common `shouldBe` Data.Set.empty
  describe "tests Regex data type" $ do
    prop "rejects invalid regexes" $ forAll (elements ['?', '+', '*']) $
      isNothing . mkRegex . Text.pack . (: [])
    prop "can build regexes from simple strings" $ forAll plainRegexInput $ \ str ->
      let text = Text.pack str
          re = mkRegex text
      in isJust re .&&. reString (fromJust re) === text
    prop "regex show is the input string" $ forAll genRegex $ \re ->
      show re === show (reString re)
    prop "regex equality on the input string" $ forAll genRegex $ \re ->
      Just re === mkRegex (reString re)
    prop "store instance is correct" $ forAll genRegex $ \re ->
      Data.Store.decode (Data.Store.encode re) === Right re
  describe "tests ImageStatus properties" $ do
    prop "PathPiece instance tests" $
      forAll (chooseEnum (minBound, maxBound)::Gen ImageStatus) $ \istatus ->
        fromPathPiece (toPathPiece istatus) === Just istatus
  describe "tests FolderClass properties" $ do
    prop "PathPiece instance tests" $
      forAll (chooseEnum (minBound, maxBound)::Gen FolderClass) $ \fclass ->
        fromPathPiece (toPathPiece fclass) === Just fclass
