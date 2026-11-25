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

module Repository.BuildSpec (spec) where

import           Data.Default
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Data.Time (LocalTime (..), midnight)

import           AtomTypes
import           Exif
import           Repository
import           Repository.Build
import           TestImport

parentSym :: SymbolizedItem
parentSym = mkSym "/pics/folder"

runLoad :: Config -> Bool -> InodeInfo -> (Image, [Image])
runLoad config isSource =
  loadImage config "folder" parentSym isSource Map.empty

runLoadExif :: Config -> Bool -> InodeInfo -> Map Text EExif -> (Image, [Image])
runLoadExif config isSource ii cache =
  loadImage config "folder" parentSym isSource cache ii

withProdNameRegexes :: Config -> Config
withProdNameRegexes config =
  config
    { cfgRangeRegex = fromMaybe (cfgRangeRegex config) $
        mkRegex "^(.*_)([0-9]+)-([0-9]+)$"
    , cfgCopyRegex = fromMaybe (cfgCopyRegex config) $
        mkRegex "^(.*)(-(close_view|copy_|publish|published|aspect_change))$"
    }

spec :: Spec
spec = parallel $ do
  describe "build file from inode" $ do
    it "builds a file from an inode with no subdirs" $ \_ -> do
      let ii = InodeInfo "file.jpg" [] False 0 0 0
          exif = def
          f = mkFileFromInode (mkSym "/pics/2022") ii exif
      fileName f `shouldBe` "file.jpg"
      fileParent f `shouldBe` mkSym "/pics/2022"
      fileDirs f `shouldBe` mkSym ""
      fileCTime f `shouldBe` 0
      fileMTime f `shouldBe` 0
      fileSize f `shouldBe` 0
      fileExif f `shouldBe` exif
      fileFullPath f `shouldBe` "/pics/2022/file.jpg"
      fileRelPath f `shouldBe` "file.jpg"
    it "builds a file from an inode" $ \_ -> do
      let ii = InodeInfo "file.jpg" ["subdir", "dir"] False 0 0 0
          exif = def
          f = mkFileFromInode (mkSym "/pics/2022") ii exif
      fileName f `shouldBe` "file.jpg"
      fileParent f `shouldBe` mkSym "/pics/2022"
      fileDirs f `shouldBe` mkSym "dir/subdir"
      fileCTime f `shouldBe` 0
      fileMTime f `shouldBe` 0
      fileSize f `shouldBe` 0
      fileExif f `shouldBe` exif
      fileFullPath f `shouldBe` "/pics/2022/dir/subdir/file.jpg"
      fileRelPath f `shouldBe` "dir/subdir/file.jpg"

  describe "implicitEventFromDateRange" $ do
    let day d = LocalTime (fromGregorian 2024 6 d) midnight
        range a b = Just (day a, day b)
        name = "folder" :: ShortText
        kindOf = extractEventType . implicitEventFromDateRange name
    it "does nothing without a date range" $ \_ ->
      kindOf Nothing `shouldBe` EKNoEvent
    it "does nothing for a same-day span" $ \_ ->
      kindOf (range 1 1) `shouldBe` EKNoEvent
    it "does nothing for a two-day span" $ \_ ->
      kindOf (range 1 3) `shouldBe` EKNoEvent
    it "uses getaway for a three-day span" $ \_ ->
      kindOf (range 1 4) `shouldBe` EKGetaway
    it "uses getaway for a six-day span" $ \_ ->
      kindOf (range 1 7) `shouldBe` EKGetaway
    it "uses grand vacation for a seven-day span" $ \_ ->
      kindOf (range 1 8) `shouldBe` EKGrandVacation
    it "uses grand vacation for a longer span" $ \_ ->
      kindOf (range 1 15) `shouldBe` EKGrandVacation
    it "marks inferred events as implicit with the folder name" $ \_ ->
      implicitEventFromDateRange name (range 1 4) `shouldBe`
        Just GetawayEvent { eventName = name, eventPeople = [], eventSource = EventImplicit implicitDateRangeDesc }

  withConfig $ do
    describe "splitPathExt and isKnownMediaInode" $ do
      it "recognises configured media extensions" $ \config -> do
        isKnownMediaInode config (mkInode "a.nef") `shouldBe` True
        isKnownMediaInode config (mkInode "a.jpg") `shouldBe` True
        isKnownMediaInode config (mkInode "a.xmp") `shouldBe` True
        isKnownMediaInode config (mkInode "a.mov") `shouldBe` True
      it "rejects unknown extensions and extensionless names" $ \config -> do
        isKnownMediaInode config (mkInode "notes.other") `shouldBe` False
        isKnownMediaInode config (mkInode "SHA1SUMS") `shouldBe` False
        isKnownMediaInode config (mkInode "corydalis.yaml") `shouldBe` False
        splitPathExt "SHA1SUMS" `shouldBe` ("SHA1SUMS", "")
        splitPathExt "a.nef" `shouldBe` ("a", "nef")
        splitPathExt "sub/a.nef" `shouldBe` ("sub/a", "nef")

    describe "loadImage" $ do
      it "treats a source raw as unprocessed" $ \config -> do
        let (img, shadows) = runLoad config True (mkInode "a.nef")
        imgName img `shouldBe` "a"
        imgStatus img `shouldBe` ImageUnprocessed
        imgType img `shouldBe` MediaImage
        isJust (imgRawPath img) `shouldBe` True
        shadows `shouldBe` []
      it "treats an output jpeg as standalone" $ \config -> do
        let (img, _) = runLoad config False (mkInode "a.jpg")
        imgStatus img `shouldBe` ImageStandalone
        null (imgJpegPath img) `shouldBe` False
        isNothing (imgRawPath img) `shouldBe` True
      it "treats a source jpeg as a soft master" $ \config -> do
        let (img, _) = runLoad config True (mkInode "a.jpg")
        flagsSoftMaster (imgFlags img) `shouldBe` True
        isJust (imgRawPath img) `shouldBe` True
        imgJpegPath img `shouldBe` []
        imgStatus img `shouldBe` ImageUnprocessed
      it "treats a sidecar-only file as orphaned without shadows" $ \config -> do
        let (img, shadows) = runLoad config False (mkInode "a.xmp")
        imgStatus img `shouldBe` ImageOrphaned
        isJust (imgSidecarPath img) `shouldBe` True
        shadows `shouldBe` []
      it "classifies source movies as master movies" $ \config -> do
        let (img, _) = runLoad config True (mkInode "clip.mov")
        imgType img `shouldBe` MediaMovie
        isJust (imgMasterMov img) `shouldBe` True
        imgMovs img `shouldBe` []
      it "classifies output movies as processed movies" $ \config -> do
        let (img, _) = runLoad config False (mkInode "clip.mp4")
        imgType img `shouldBe` MediaMovie
        imgMasterMov img `shouldBe` Nothing
        null (imgMovs img) `shouldBe` False
      it "classifies unknown extensions as untracked" $ \config -> do
        let (img, _) = runLoad config True (mkInode "notes.other")
        imgType img `shouldBe` MediaUnknown
        null (imgUntracked img) `shouldBe` False
      it "classifies extensionless files as untracked" $ \config -> do
        let (img, _) = runLoad config True (mkInode "SHA1SUMS")
        imgType img `shouldBe` MediaUnknown
        null (imgUntracked img) `shouldBe` False
      it "expands range jpegs into shadows" $ \config -> do
        let (img, shadows) = runLoad (withProdNameRegexes config) False (mkInode "a_1-3.jpg")
        imgName img `shouldBe` "a_1-3"
        imgRange img `shouldBe` Just (ImageRange "a_" 1 3 1)
        map imgName shadows `shouldBe` ["a_1", "a_2", "a_3"]
      it "gives a movie named like a range no range" $ \config -> do
        let (img, shadows) = runLoad config False (mkInode "a_1-3.mov")
        imgName img `shouldBe` "a_1-3"
        imgRange img `shouldBe` Nothing
        map imgName shadows `shouldBe` []
      it "drops copy suffixes from the image name" $ \config -> do
        let (img, _) = runLoad config True (mkInode "foo-1.nef")
        imgName img `shouldBe` "foo"
      it "warns when exif is missing from the cache" $ \config -> do
        let (img, _) = runLoad config True (mkInode "a.nef")
        imgProblems img `shouldBe` Set.singleton "exif: Internal error: exif not read"
      it "warns when exif reading failed" $ \config -> do
        let cache = Map.singleton "a.nef" (Left "boom")
            (img, _) = runLoadExif config True (mkInode "a.nef") cache
        imgProblems img `shouldBe` Set.singleton "exif: Cannot read exif: boom"
      it "attaches a successful exif entry" $ \config -> do
        let ex = datedExif 2021 3 4
            cache = Map.singleton "a.nef" (Right ex)
            (img, _) = runLoadExif config True (mkInode "a.nef") cache
        imageYear img `shouldBe` Just 2021
        imageYearMonth img `shouldBe` Just (2021, 3)
      it "classifies nested inodes as raw with relative dirs" $ \config -> do
        let ii = (mkInode "a.nef") { inodeDirs = ["sub"] }
            (img, _) = runLoad config True ii
        imgName img `shouldBe` "sub/a"
        fileRelPath <$> imgRawPath img `shouldBe` Just "sub/a.nef"

    describe "buildFolderFromInodes" $ do
      it "assembles a folder from raw inodes" $ \config -> do
        let folder = buildFolderFromInodes config "2024-01-01" "/pics/2024-01-01" True
                       [mkInode "a.nef"] Map.empty Nothing
        pdName folder `shouldBe` "2024-01-01"
        imgStatus (pdImages folder Map.! "a") `shouldBe` ImageUnprocessed
        pdEvent folder `shouldBe` Nothing
      it "keeps an explicit yaml event" $ \config -> do
        let ev = BirthdayEvent { eventName = "Birthday", eventPeople = [], eventSource = EventExplicit Nothing }
            folder = buildFolderFromInodes config "folder" "/p" True [mkInode "a.nef"] Map.empty (Just ev)
        pdEvent folder `shouldBe` Just ev
      it "infers an implicit event from a multi-day date range" $ \config -> do
        let e1 = datedExif 2024 6 1
            e2 = datedExif 2024 6 5
            cache = Map.fromList [("a.nef", Right e1), ("b.nef", Right e2)]
            folder = buildFolderFromInodes config "folder" "/p" True
                       [mkInode "a.nef", mkInode "b.nef"] cache Nothing
        extractEventType (pdEvent folder) `shouldBe` EKGetaway
      it "dates a folder by its earliest dated image" $ \config -> do
        let cache = Map.fromList [ ("a.nef", Right (datedExif 2024 6 5))
                                 , ("c.nef", Right (datedExif 2024 6 1))
                                 , ("e.nef", Right (datedExif 2024 6 3))
                                 ]
            -- With undated images before, between and after.
            names = ["0.nef", "a.nef", "b.nef", "c.nef", "d.nef", "e.nef", "f.nef"]
            folder = buildFolderFromInodes config "folder" "/p" True
                       (map mkInode names) cache Nothing
        Map.size (pdImages folder) `shouldBe` length names
        pdTimestamp folder `shouldBe` Just (LocalTime (fromGregorian 2024 6 1) midnight)
        pdYear folder `shouldBe` Just 2024
        let undated = buildFolderFromInodes config "folder" "/p" True
                        [mkInode "a.nef"] Map.empty Nothing
        pdTimestamp undated `shouldBe` Nothing
      it "keeps unknown and extensionless files out of the image map" $ \config -> do
        let eImg = datedExif 2024 6 1
            eOther = datedExif 1999 1 1
            cache = Map.fromList
              [ ("a.nef", Right eImg)
              , ("notes.other", Right eOther)
              , ("SHA1SUMS", Right eOther)
              , ("corydalis.yaml", Right eOther)
              ]
            folder = buildFolderFromInodes config "folder" "/p" True
                       [ mkInode "a.nef"
                       , mkInode "notes.other"
                       , mkInode "SHA1SUMS"
                       , mkInode "corydalis.yaml"
                       ] cache Nothing
        Map.keys (pdImages folder) `shouldBe` ["a"]
        map fileName (pdUntracked folder) `shouldMatchList`
          ["notes.other", "SHA1SUMS", "corydalis.yaml"]
        let captured = LocalTime (fromGregorian 2024 6 1) midnight
        pdTimestamp folder `shouldBe` Just captured
        sDateRange (pdStats folder) `shouldBe` Just (captured, captured)
        sUntracked (pdStats folder) `shouldBe` 3
        Map.notMember "notes" (pdImages folder) `shouldBe` True
        Map.notMember "SHA1SUMS" (pdImages folder) `shouldBe` True
        Map.notMember "corydalis" (pdImages folder) `shouldBe` True
      it "does not attach unknown same-basename files to images" $ \config -> do
        let folder = buildFolderFromInodes config "folder" "/p" True
                       [mkInode "a.nef", mkInode "a.txt"] Map.empty Nothing
        imgUntracked (pdImages folder Map.! "a") `shouldBe` []
        map fileName (pdUntracked folder) `shouldBe` ["a.txt"]

    describe "isBetterMaster" $ do
      it "prefers earlier extensions" $ \_ -> do
        isBetterMaster ["nef", "raf"] "nef" "raf" `shouldBe` True
        isBetterMaster ["nef", "raf"] "raf" "nef" `shouldBe` False
      it "chooses the first file when neither extension matches" $ \_ ->
        isBetterMaster ["nef"] "xxx" "yyy" `shouldBe` True
      it "chooses the first file on an empty extension list" $ \_ ->
        isBetterMaster [] "a" "b" `shouldBe` True

    describe "selectMasterFile and mergePictures" $ do
      it "keeps a hard master over a soft one" $ \config -> do
        let hard = simpleRawImage config
            jpeg = simpleFile "a.jpg"
            soft = mkImage config "a" "test" (Just jpeg) Nothing [] Nothing [] [] Nothing MediaImage (Flags True)
            (chosen, softFlag, extra) = selectMasterFile (cfgRawExts config) imgRawPath hard soft
        chosen `shouldBe` imgRawPath hard
        softFlag `shouldBe` False
        extra `shouldBe` [jpeg]
      it "merges raw and jpeg into a processed image" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            merged = mergePictures config raw jpegImg
        imgStatus merged `shouldBe` ImageProcessed
        isJust (imgRawPath merged) `shouldBe` True
        null (imgJpegPath merged) `shouldBe` False
      it "selects nef over raf as the real master" $ \config -> do
        let nef = mkImage config "a" "test" (Just $ simpleFile "a.nef") Nothing [] Nothing [] [] Nothing MediaImage def
            raf = mkImage config "a" "test" (Just $ simpleFile "a.raf") Nothing [] Nothing [] [] Nothing MediaImage def
            merged = mergePictures config nef raf
        fileName <$> imgRawPath merged `shouldBe` Just "a.nef"
        map fileName (imgJpegPath merged) `shouldBe` ["a.raf"]

    describe "mergeFolders" $ do
      it "keeps the path with more raw files as main" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "b" "trip" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def
            d1 = (picDirWith "trip" [raw]) { pdMainPath = "/raw/trip" }
            d2 = (picDirWith "trip" [jpegImg]) { pdMainPath = "/jpg/trip" }
            merged = mergeFolders config d1 d2
        pdMainPath merged `shouldBe` "/raw/trip"
        pdSecPaths merged `shouldContain` ["/jpg/trip"]
      it "recomputes stats after raw and jpeg of the same image merge" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            merged = mergeFolders config (picDirWith "test" [raw]) (picDirWith "test" [jpegImg])
        imgStatus (pdImages merged Map.! "a") `shouldBe` ImageProcessed
        sProcessed (pdStats merged) `shouldBe` 1
        sRaw (pdStats merged) `shouldBe` 0
        sStandalone (pdStats merged) `shouldBe` 0
      it "concatenates untracked files and counts them in stats" $ \config -> do
        let d1 = (picDirWith "test" [simpleRawImage config])
                   { pdUntracked = [fileSized "SHA1SUMS" 10] }
            d2 = (createTestPicDir "test")
                   { pdUntracked = [fileSized "notes.other" 7] }
            merged = mergeFolders config d1 d2
        map fileName (pdUntracked merged) `shouldMatchList` ["SHA1SUMS", "notes.other"]
        sUntracked (pdStats merged) `shouldBe` 2
        sUntrackedSize (pdStats merged) `shouldBe` 17
        Map.member "a" (pdImages merged) `shouldBe` True

    describe "mergeShadows and ranges" $ do
      it "applies shadows onto matching images" $ \config -> do
        let raw = simpleRawImage config
            shadow = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            dir = (createTestPicDir "test")
                    { pdImages = Map.singleton "a" raw
                    , pdShadows = Map.singleton "a" shadow
                    }
            merged = mergeShadows config dir
        imgStatus (pdImages merged Map.! "a") `shouldBe` ImageProcessed
      it "promotes a standalone range jpeg when the begin image has a raw" $ \config -> do
        let root = mkImage config "a_1" "test" (Just $ simpleFile "a_1.nef") Nothing []
                           Nothing [] [] Nothing MediaImage def
            ranged = mkImage config "a_1-3" "test" Nothing Nothing [simpleFile "a_1-3.jpg"]
                             Nothing [] [] (Just (ImageRange "a_" 1 3 1)) MediaImage def
            dir = picDirWith "test" [root, ranged]
        imgStatus ranged `shouldBe` ImageStandalone
        case maybeUpdateStandaloneRange config dir ranged of
          Nothing -> expectationFailure "expected standalone range to be updated"
          Just img -> do
            imgStatus img `shouldBe` ImageProcessed
            isJust (imgRawPath img) `shouldBe` True
      it "does nothing when the range begin has no raw" $ \config -> do
        let begin = mkImage config "a_1" "test" Nothing Nothing [simpleFile "a_1.jpg"] Nothing [] [] Nothing MediaImage def
            ranged = mkImage config "a_1-3" "test" Nothing Nothing [simpleFile "a_1-3.jpg"]
                             Nothing [] [] (Just (ImageRange "a_" 1 3 1)) MediaImage def
            dir = picDirWith "test" [begin, ranged]
        maybeUpdateStandaloneRange config dir ranged `shouldBe` Nothing
      it "rewrites standalone range images in the folder" $ \config -> do
        let root = mkImage config "a_1" "test" (Just $ simpleFile "a_1.nef") Nothing []
                           Nothing [] [] Nothing MediaImage def
            ranged = mkImage config "a_1-3" "test" Nothing Nothing [simpleFile "a_1-3.jpg"]
                             Nothing [] [] (Just (ImageRange "a_" 1 3 1)) MediaImage def
            dir = resolveProcessedRanges config (picDirWith "test" [root, ranged])
        imgStatus (pdImages dir Map.! "a_1-3") `shouldBe` ImageProcessed

  withContext $ do
    describe "addDirToRepo event merge" $ do
      let implicitEv = Just GrandVacationEvent
            { eventName = "trip"
            , eventPeople = []
            , eventSource = EventImplicit implicitDateRangeDesc
            }
          explicitEv = Just BirthdayEvent
            { eventName = "trip"
            , eventPeople = []
            , eventSource = EventExplicit (Just "corydalis.yaml")
            }
          picDirnoEvent = createTestPicDir "trip"
          picDirImplicitEvent = picDirnoEvent { pdEvent = implicitEv }
          picDirExplicitEvent = picDirnoEvent { pdEvent = explicitEv }
      it "keeps an event when merging a new folder into one without" $ \ctx -> do
        let mergedImpl = addDirToRepo (ctxConfig ctx) picDirImplicitEvent (Map.singleton "trip" picDirnoEvent)
            mergedExpl = addDirToRepo (ctxConfig ctx) picDirExplicitEvent (Map.singleton "trip" picDirnoEvent)
        pdEvent (mergedImpl Map.! "trip") `shouldBe` implicitEv
        pdEvent (mergedExpl Map.! "trip") `shouldBe` explicitEv
      it "keeps an event when merging a folder without one into one with" $ \ctx -> do
        let mergedImpl = addDirToRepo (ctxConfig ctx) picDirnoEvent (Map.singleton "trip" picDirImplicitEvent)
            mergedExpl = addDirToRepo (ctxConfig ctx) picDirnoEvent (Map.singleton "trip" picDirExplicitEvent)
        pdEvent (mergedImpl Map.! "trip") `shouldBe` implicitEv
        pdEvent (mergedExpl Map.! "trip") `shouldBe` explicitEv
      it "prefers an explicit event over an implicit one" $ \ctx -> do
        let mergedNew = addDirToRepo (ctxConfig ctx) picDirExplicitEvent (Map.singleton "trip" picDirImplicitEvent)
            mergedOld = addDirToRepo (ctxConfig ctx) picDirImplicitEvent (Map.singleton "trip" picDirExplicitEvent)
        pdEvent (mergedNew Map.! "trip") `shouldBe` explicitEv
        pdEvent (mergedOld Map.! "trip") `shouldBe` explicitEv
