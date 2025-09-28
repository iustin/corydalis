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
{-# LANGUAGE ViewPatterns      #-}

module TypesSpec (spec) where

import           TestImport
import           Types

import           ClassyPrelude.Yesod
import           Data.Aeson
import           Data.Maybe          (fromJust)
import qualified Data.Set
import           Data.Store
import qualified Data.Text           as Text
import           Data.Text.Arbitrary ()
import           Data.Time
import           Formatting
import           Test.QuickCheck
import           Text.Regex.TDFA     (matchTest)

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
      let txt = Text.pack str
          re = mkRegex txt
      in isJust re .&&. reString (fromJust re) === txt
    prop "regex show is the input string" $ forAll genRegex $ \re ->
      show re === show (reString re)
    prop "regex equality on the input string" $ forAll genRegex $ \re ->
      Just re === mkRegex (reString re)
    prop "store instance is correct" $ forAll genRegex $ \re ->
      Data.Store.decode (Data.Store.encode re) === Right re
    it "correctly builds a basic string" $ do
      let re = mkRegex "a.c"
      case re of
        Nothing -> expectationFailure "Can't build basic regex"
        Just (reRegex -> r) -> do
          matchTest r ("abc"::String) `shouldBe` True
          matchTest r ("bcd"::String) `shouldBe` False
  describe "tests JSDiffTime properties" $ do
    prop "JSON basic decoding" $ \n ->
      decodeStrictText (sformat int n) ===
        Just (JSDiffTime (secondsToNominalDiffTime . fromIntegral $ (n::Int)))
  describe "tests ImageStatus properties" $ do
    prop "PathPiece instance tests" $
      forAll (chooseEnum (minBound, maxBound)::Gen ImageStatus) $ \istatus ->
        fromPathPiece (toPathPiece istatus) === Just istatus
    prop "Storable instance tests" $
      forAll (chooseEnum (minBound, maxBound)::Gen ImageStatus) $ \istatus ->
        Data.Store.decode (Data.Store.encode istatus) === Right istatus
  describe "tests FolderClass properties" $ do
    prop "PathPiece instance tests" $
      forAll (chooseEnum (minBound, maxBound)::Gen FolderClass) $ \fclass ->
        fromPathPiece (toPathPiece fclass) === Just fclass
  describe "tests Progress properties" $ do
    it "check basic operation" $ do
      let pgzero = def::Progress
          pgone = pgzero { pgGoal = 1}
          pgdone = incDone pgone
      pgProgress pgzero `shouldBe` Nothing
      pgProgress pgone `shouldBe` Just 0
      pgProgress pgdone `shouldBe` Just 1
      pgTotal (incDone pgzero) `shouldBe` 1
      pgTotal (incErrors "a" "b" pgzero) `shouldBe` 1
      pgTotal (incNoop pgzero) `shouldBe` 1
    prop "incError construction" $ \item err ->
      let pg = incErrors item err def
      in case pgErrors pg of
        [p] -> peItem p === item .&&. peError p === err
        _   -> counterexample "Unexpected error count" False
    prop "arbitrary increases" $ \errs noop dones ->
      pgTotal (incProgress (map (uncurry ProgressError) errs) noop dones def)
        === (length errs + noop + dones)
  describe "tests ViewMode properties" $ do
    prop "parse/format equivalence" $
      forAll (chooseEnum (minBound, maxBound)::Gen ViewMode) $ \vm ->
        parseViewMode (decodeUtf8 $ formatViewMode vm) === Just vm
