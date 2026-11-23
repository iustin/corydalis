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
    it "builds the combined media extension set" $ \ctx -> do
      let c = ctxConfig ctx
          expected = Data.Set.unions
            [ cfgRawExtsSet c, cfgJpegExts c, cfgSidecarExts c, cfgMovieExts c ]
      cfgAllMediaExts c `shouldBe` expected
    it "uses the default image decoders if none are configured" $ \ctx ->
      cfgImageDecoders (ctxConfig ctx) `shouldBe` defaultImageDecoders
    it "uses the default movie demuxers if none are configured" $ \ctx ->
      cfgMovieDemuxers (ctxConfig ctx) `shouldBe` defaultMovieDemuxers
  describe "checks image decoders parsing" $ do
    let parse :: [(Text, Text)] -> Maybe (Map Text Text)
        parse = parseImageDecoders . mapFromList
    it "accepts the default decoders" $
      parseImageDecoders defaultImageDecoders `shouldBe` Just defaultImageDecoders
    it "lower-cases extensions and decoders" $
      parse [("JPG", "JPEG"), ("Heic", "heic")] `shouldBe`
        Just (mapFromList [("jpg", "jpeg"), ("heic", "heic")])
    it "accepts an empty map" $
      parse [] `shouldBe` Just mempty
    forM_ ["ps", "EPS", "pdf", "svg", "mvg", "msl", "text", "url"] $ \decoder ->
      it ("rejects the unsafe decoder " ++ show decoder) $
        parse [("jpg", decoder)] `shouldBe` Nothing
    forM_ ["", "jpeg:", "jpeg:/etc/passwd", "@x", "jp eg", "jpeg\n"] $ \decoder ->
      it ("rejects the invalid decoder " ++ show decoder) $
        parse [("jpg", decoder)] `shouldBe` Nothing
    forM_ ["", ".jpg", "a/b", "j:pg"] $ \ext ->
      it ("rejects the invalid extension " ++ show ext) $
        parse [(ext, "jpeg")] `shouldBe` Nothing
    it "rejects extensions duplicated when ignoring case" $
      parse [("jpg", "jpeg"), ("JPG", "png")] `shouldBe` Nothing
  describe "checks movie demuxers parsing" $ do
    let parse :: [(Text, Text)] -> Maybe (Map Text Text)
        parse = parseMovieDemuxers . mapFromList
    it "accepts the default demuxers" $
      parseMovieDemuxers defaultMovieDemuxers `shouldBe` Just defaultMovieDemuxers
    it "only uses safe demuxers by default" $
      all (`member` safeMovieDemuxers) (toList defaultMovieDemuxers) `shouldBe` True
    it "lower-cases extensions and demuxers" $
      parse [("MP4", "MOV"), ("Mkv", "matroska")] `shouldBe`
        Just (mapFromList [("mp4", "mov"), ("mkv", "matroska")])
    it "accepts an empty map" $
      parse [] `shouldBe` Just mempty
    forM_ ["hls", "concat", "lavfi", "image2", "tty", "sdp", "rtsp", "data"] $ \demuxer ->
      it ("rejects the demuxer " ++ show demuxer) $
        parse [("mp4", demuxer)] `shouldBe` Nothing
    forM_ ["", "mov:", "mov,mp4", "m ov", "mov\n"] $ \demuxer ->
      it ("rejects the invalid demuxer " ++ show demuxer) $
        parse [("mp4", demuxer)] `shouldBe` Nothing
    forM_ ["", ".mp4", "a/b", "m:p4"] $ \ext ->
      it ("rejects the invalid extension " ++ show ext) $
        parse [(ext, "mov")] `shouldBe` Nothing
    it "rejects extensions duplicated when ignoring case" $
      parse [("mp4", "mov"), ("MP4", "avi")] `shouldBe` Nothing
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
      pgWork (incDone pgone) `shouldBe` 1
      pgWork (incNoop pgone) `shouldBe` 0
      pgWorkRemaining (incDone pgone) `shouldBe` 0
      pgWorkRemaining pgone `shouldBe` 1
      pgWorkProgress pgzero `shouldBe` Nothing
      pgWorkProgress pgone `shouldBe` Just 0
      pgWorkProgress pgdone `shouldBe` Just 1
      pgWorkProgress (incNoop pgzero) `shouldBe` Just 1
    it "caps the progress at the whole when the goal is exceeded" $ do
      -- Goals counted in advance can be exceeded.
      let over = (def::Progress) { pgGoal = 1, pgDone = 2, pgNoop = 1 }
      pgProgress over `shouldBe` Just 1
      pgWorkProgress over `shouldBe` Just 1
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
  describe "tests SymbolizedItem properties" $ do
    prop "to/from Text equivalence" $ \txt ->
      deSymbolizeItem (mkSymbolizedItem txt) === txt
    prop "to/from Text equivalence with Maybe" $ \mtxt ->
      maybeDesymbolizeItem (fmap mkSymbolizedItem mtxt) === mtxt
    it "checks symbolized equivalence" $ do
      let t1 = "abc"::Text
          t2 = "abc"::Text
          s1 = mkSymbolizedItem t1
          s2 = mkSymbolizedItem t2
      s1 `shouldBe` s2
  describe "tests Event NFData" $ do
    it "forces event fields and sources" $ do
      let ev = GenericEvent "trip" [] (EventExplicit Nothing)
      evaluate (rnf ev) `shouldReturn` ()
      evaluate (GenericEvent (error "name") [] (EventExplicit Nothing))
        `shouldThrow` anyErrorCall
      evaluate (rnf (ev { eventPeople = [error "person"] }))
        `shouldThrow` anyErrorCall
      evaluate (EventImplicit (error "src"))
        `shouldThrow` anyErrorCall
      evaluate (rnf (EventExplicit (Just (error "path"))))
        `shouldThrow` anyErrorCall
