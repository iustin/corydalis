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
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text.Short  as TS
import           Data.Time (LocalTime (..), midnight)

import           AtomTypes
import           Exif
import           Pics              (imageViewSource)
import           Repository
import           Repository.Build
import           System.Posix.Types (FileOffset)
import           TestImport

parentSym :: SymbolizedItem
parentSym = mkSym "/pics/folder"

runLoad :: Config -> Bool -> InodeInfo -> Image
runLoad config isSource =
  loadImage config "folder" parentSym isSource Map.empty

runLoadExif :: Config -> Bool -> InodeInfo -> Map Text EExif -> Image
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

-- | The directories of a folder named "f", built as a scan does, from
-- the files (name and size) of its source and output directories,
-- without exif.
compositeDirs :: Config -> [[(FilePath, FileOffset)]] -> [[(FilePath, FileOffset)]] -> [PicDir]
compositeDirs = compositeDirsWith Map.empty

-- | As 'compositeDirs', with the exif of the files named in the cache.
compositeDirsWith :: Map Text EExif -> Config -> [[(FilePath, FileOffset)]] -> [[(FilePath, FileOffset)]] -> [PicDir]
compositeDirsWith cache config srcs outs =
  let mk isSrc i files =
        buildFolderFromInodes config "f"
          ((if isSrc then "/src" else "/out") <> show (i :: Int) <> "/f") isSrc
          (map (\(n, size) -> (mkInode n) { inodeSize = size }) files) cache Nothing
  in zipWith (mk True) [1..] srcs ++ zipWith (mk False) [1..] outs

-- | The finished folder "f" (see 'compositeDirs').
compositeFolder :: Config -> [[(FilePath, FileOffset)]] -> [[(FilePath, FileOffset)]] -> PicDir
compositeFolder = compositeFolderWith Map.empty

-- | The finished folder "f" (see 'compositeDirsWith').
compositeFolderWith :: Map Text EExif -> Config -> [[(FilePath, FileOffset)]] -> [[(FilePath, FileOffset)]] -> PicDir
compositeFolderWith cache config srcs outs =
  finishFolder $ foldl' (flip (addDirToRepo config)) Map.empty (compositeDirsWith cache config srcs outs) Map.! "f"

-- | Checks that a finished folder's links agree with the images'
-- ranges, and go both ways: exactly the images with a range are
-- composite images, and a composite image lists a component exactly
-- when the component lists it.
linksAreConsistent :: PicDir -> Expectation
linksAreConsistent dir =
  forM_ (pdImages dir) $ \img -> do
    let name = imgName img
        roleOf n = imgRole <$> Map.lookup n (pdImages dir)
    case imgRole img of
      CompositeOf components -> do
        isJust (imgRange img) `shouldBe` True
        forM_ components $ \c -> case roleOf c of
          Just (ComponentOf composites) -> toList composites `shouldContain` [name]
          other -> expectationFailure $ show c ++ " is not a component: " ++ show other
      ComponentOf composites -> do
        isJust (imgRange img) `shouldBe` False
        forM_ composites $ \c -> case roleOf c of
          Just (CompositeOf components) -> components `shouldContain` [name]
          other -> expectationFailure $ show c ++ " is not a composite: " ++ show other
      PlainImage -> isJust (imgRange img) `shouldBe` False

-- | A folder's stored counts: unprocessed, standalone, processed and
-- movies.
folderCounts :: PicDir -> (Int, Int, Int, Int)
folderCounts dir = let s = pdStats dir in (sRaw s, sStandalone s, sProcessed s, sMovies s)

-- | A folder's stored sizes: raw, processed, standalone and movies.
folderSizes :: PicDir -> (FileOffset, FileOffset, FileOffset, FileOffset)
folderSizes dir =
  let s = pdStats dir in (sRawSize s, sProcSize s, sStandaloneSize s, sMovieSize s)

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
        let img = runLoad config True (mkInode "a.nef")
        imgName img `shouldBe` "a"
        imgStatus img `shouldBe` ImageUnprocessed
        imgType img `shouldBe` MediaImage
        isJust (imgRawPath img) `shouldBe` True
      it "treats an output jpeg as standalone" $ \config -> do
        let img = runLoad config False (mkInode "a.jpg")
        imgStatus img `shouldBe` ImageStandalone
        null (imgJpegPath img) `shouldBe` False
        isNothing (imgRawPath img) `shouldBe` True
      it "treats a source jpeg as a soft master" $ \config -> do
        let img = runLoad config True (mkInode "a.jpg")
        flagsSoftMaster (imgFlags img) `shouldBe` True
        isJust (imgRawPath img) `shouldBe` True
        imgJpegPath img `shouldBe` []
        imgStatus img `shouldBe` ImageUnprocessed
      it "treats a sidecar-only file as orphaned" $ \config -> do
        let img = runLoad config False (mkInode "a.xmp")
        imgStatus img `shouldBe` ImageOrphaned
        isJust (imgSidecarPath img) `shouldBe` True
      it "classifies source movies as master movies" $ \config -> do
        let img = runLoad config True (mkInode "clip.mov")
        imgType img `shouldBe` MediaMovie
        isJust (imgMasterMov img) `shouldBe` True
        imgMovs img `shouldBe` []
      it "classifies output movies as processed movies" $ \config -> do
        let img = runLoad config False (mkInode "clip.mp4")
        imgType img `shouldBe` MediaMovie
        imgMasterMov img `shouldBe` Nothing
        null (imgMovs img) `shouldBe` False
      it "classifies unknown extensions as untracked" $ \config -> do
        let img = runLoad config True (mkInode "notes.other")
        imgType img `shouldBe` MediaUnknown
        null (imgUntracked img) `shouldBe` False
      it "classifies extensionless files as untracked" $ \config -> do
        let img = runLoad config True (mkInode "SHA1SUMS")
        imgType img `shouldBe` MediaUnknown
        null (imgUntracked img) `shouldBe` False
      it "reads the range of a range jpeg, as one image" $ \config -> do
        let img = runLoad (withProdNameRegexes config) False (mkInode "a_1-3.jpg")
        imgName img `shouldBe` "a_1-3"
        imgRange img `shouldBe` Just (ImageRange "a_" 1 3 1)
        -- Linked only when the folder is finished.
        imgRole img `shouldBe` PlainImage
      it "gives a movie named like a range no range" $ \config -> do
        let img = runLoad config False (mkInode "a_1-3.mov")
        imgName img `shouldBe` "a_1-3"
        imgRange img `shouldBe` Nothing
      it "drops copy suffixes from the image name" $ \config -> do
        let img = runLoad config True (mkInode "foo-1.nef")
        imgName img `shouldBe` "foo"
      it "warns when exif is missing from the cache" $ \config -> do
        let img = runLoad config True (mkInode "a.nef")
        imgProblems img `shouldBe` Set.singleton "exif: Internal error: exif not read"
      it "warns when exif reading failed" $ \config -> do
        let cache = Map.singleton "a.nef" (Left "boom")
            img = runLoadExif config True (mkInode "a.nef") cache
        imgProblems img `shouldBe` Set.singleton "exif: Cannot read exif: boom"
      it "attaches a successful exif entry" $ \config -> do
        let ex = datedExif 2021 3 4
            cache = Map.singleton "a.nef" (Right ex)
            img = runLoadExif config True (mkInode "a.nef") cache
        imageYear img `shouldBe` Just 2021
        imageYearMonth img `shouldBe` Just (2021, 3)
      it "classifies nested inodes as raw with relative dirs" $ \config -> do
        let ii = (mkInode "a.nef") { inodeDirs = ["sub"] }
            img = runLoad config True ii
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
            folder = finishFolder $ buildFolderFromInodes config "folder" "/p" True
                       [mkInode "a.nef", mkInode "b.nef"] cache Nothing
        extractEventType (pdEvent folder) `shouldBe` EKGetaway
      it "dates a folder by its earliest dated image" $ \config -> do
        let cache = Map.fromList [ ("a.nef", Right (datedExif 2024 6 5))
                                 , ("c.nef", Right (datedExif 2024 6 1))
                                 , ("e.nef", Right (datedExif 2024 6 3))
                                 ]
            -- With undated images before, between and after.
            names = ["0.nef", "a.nef", "b.nef", "c.nef", "d.nef", "e.nef", "f.nef"]
            folder = finishFolder $ buildFolderFromInodes config "folder" "/p" True
                       (map mkInode names) cache Nothing
        Map.size (pdImages folder) `shouldBe` length names
        pdTimestamp folder `shouldBe` Just (LocalTime (fromGregorian 2024 6 1) midnight)
        pdYear folder `shouldBe` Just 2024
      it "dates a folder over all its directories, once merged" $ \config -> do
        let src = buildFolderFromInodes config "folder" "/src/folder" True [mkInode "a.nef"]
                    (Map.fromList [("a.nef", Right (datedExif 2023 6 20))]) Nothing
            out = buildFolderFromInodes config "folder" "/out/folder" False [mkInode "b.jpg"]
                    (Map.fromList [("b.jpg", Right (datedExif 2023 6 1))]) Nothing
            folder = finishFolder $ foldl' (flip (addDirToRepo config)) Map.empty [src, out] Map.! "folder"
        pdTimestamp folder `shouldBe` Just (LocalTime (fromGregorian 2023 6 1) midnight)
        pdYear folder `shouldBe` Just 2023
        -- The event spans both directories' dates.
        extractEventType (pdEvent folder) `shouldBe` EKGrandVacation
      it "keeps an explicit yaml event over the dates of a finished folder" $ \config -> do
        let ev = BirthdayEvent { eventName = "Birthday", eventPeople = [], eventSource = EventExplicit Nothing }
            cache = Map.fromList [("a.nef", Right (datedExif 2024 6 1)), ("b.nef", Right (datedExif 2024 6 20))]
            folder = finishFolder $ buildFolderFromInodes config "folder" "/p" True
                       [mkInode "a.nef", mkInode "b.nef"] cache (Just ev)
        pdEvent folder `shouldBe` Just ev
      it "computes the implicit event again when finishing a folder again" $ \config -> do
        let cache = Map.fromList [("a.nef", Right (datedExif 2024 6 1)), ("b.nef", Right (datedExif 2024 6 4))]
            folder = finishFolder $ buildFolderFromInodes config "folder" "/p" True
                       [mkInode "a.nef", mkInode "b.nef"] cache Nothing
            later = mkImage config "c" "folder" Nothing Nothing [simpleFile "c.jpg"] Nothing [] [] Nothing MediaImage def
            dated = later { imgExif = datedExif 2024 6 20 }
            again = finishFolder folder { pdImages = Map.insert "c" dated (pdImages folder) }
        extractEventType (pdEvent folder) `shouldBe` EKGetaway
        extractEventType (pdEvent again) `shouldBe` EKGrandVacation
        let undated = finishFolder $ buildFolderFromInodes config "folder" "/p" True
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
            folder = finishFolder $ buildFolderFromInodes config "folder" "/p" True
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
      it "keeps the directory with more masters as main, before finishing" $ \config -> do
        -- Directories as a scan builds them, with no stats yet.
        let raws = buildFolderFromInodes config "trip" "/raw/trip" True
                     [mkInode "a.nef"] Map.empty Nothing
            jpegs = buildFolderFromInodes config "trip" "/jpg/trip" False
                      [mkInode "b.jpg", mkInode "c.jpg"] Map.empty Nothing
        pdMainPath (mergeFolders config raws jpegs) `shouldBe` "/raw/trip"
        pdMainPath (mergeFolders config jpegs raws) `shouldBe` "/raw/trip"
      it "keeps the path with more raw files as main" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "b" "trip" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def
            d1 = (picDirWith "trip" [raw]) { pdMainPath = "/raw/trip" }
            d2 = (picDirWith "trip" [jpegImg]) { pdMainPath = "/jpg/trip" }
            merged = mergeFolders config d1 d2
        pdMainPath merged `shouldBe` "/raw/trip"
        pdSecPaths merged `shouldContain` ["/jpg/trip"]
      it "counts raw and jpeg of the same image as one processed image" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            merged = finishFolder $ mergeFolders config (picDirWith "test" [raw]) (picDirWith "test" [jpegImg])
        imgStatus (pdImages merged Map.! "a") `shouldBe` ImageProcessed
        sProcessed (pdStats merged) `shouldBe` 1
        sRaw (pdStats merged) `shouldBe` 0
        sStandalone (pdStats merged) `shouldBe` 0
      it "concatenates untracked files and counts them in stats" $ \config -> do
        let d1 = (picDirWith "test" [simpleRawImage config])
                   { pdUntracked = [fileSized "SHA1SUMS" 10] }
            d2 = (createTestPicDir "test")
                   { pdUntracked = [fileSized "notes.other" 7] }
            merged = finishFolder $ mergeFolders config d1 d2
        map fileName (pdUntracked merged) `shouldMatchList` ["SHA1SUMS", "notes.other"]
        sUntracked (pdStats merged) `shouldBe` 2
        sUntrackedSize (pdStats merged) `shouldBe` 17
        Map.member "a" (pdImages merged) `shouldBe` True

    describe "composite images" $ do
      -- The cases of docs/composite-images.md: one folder, with three
      -- raws in a source directory unless said otherwise. The sizes are
      -- made up, to show double counting: a raw is 10, the range JPEG
      -- 50, a TIFF or DNG 100, a component's own JPEG 5, a movie 70.
      let nefs = [("d_01.nef", 10), ("d_02.nef", 10), ("d_03.nef", 10)]
          check config srcs outs counts sizes = do
            let dir = compositeFolder config srcs outs
            folderCounts dir `shouldBe` counts
            folderSizes dir `shouldBe` sizes
            -- The stored numbers are the final ones.
            show (pdStats dir) `shouldBe` show (computeFolderStats dir)
            linksAreConsistent dir
            return dir
          roleOf dir name = imgRole (pdImages dir Map.! name)
          statusOf dir name = imgStatus (pdImages dir Map.! name)
          viewedFrom config dir name =
            fileName . fst <$> imageViewSource config (pdImages dir Map.! name)
      it "links a range jpeg with its raws (case 1)" $ \config -> do
        dir <- check config [nefs] [[("d_01-03.jpg", 50)]] (0, 0, 4, 0) (30, 50, 0, 0)
        roleOf dir "d_01-03" `shouldBe` CompositeOf ["d_01", "d_02", "d_03"]
        forM_ ["d_01", "d_02", "d_03"] $ \name -> do
          roleOf dir name `shouldBe` ComponentOf ("d_01-03" :| [])
          -- Each component is viewed from its own raw.
          viewedFrom config dir name `shouldBe` Just (TS.fromString (TS.toString (unImageName name) <> ".nef"))
        viewedFrom config dir "d_01-03" `shouldBe` Just "d_01-03.jpg"
      it "counts an intermediate file as the master (case 2)" $ \config -> do
        dir <- check config [nefs ++ [("d_01-03.tif", 100)]] [[("d_01-03.jpg", 50)]]
                 (0, 0, 4, 0) (130, 50, 0, 0)
        viewedFrom config dir "d_01" `shouldBe` Just "d_01.nef"
      it "covers the components as soon as the composite exists (case 3)" $ \config -> do
        dir <- check config [nefs ++ [("d_01-03.tif", 100)]] [] (1, 0, 3, 0) (130, 0, 0, 0)
        statusOf dir "d_01-03" `shouldBe` ImageUnprocessed
        statusOf dir "d_01" `shouldBe` ImageProcessed
      it "keeps the links whatever directory has the composite (cases 4, 5)" $ \config -> do
        void $ check config [nefs ++ [("d_01-03.tif", 100)]] [[("d_01.jpg", 5)]]
                 (1, 0, 3, 0) (130, 5, 0, 0)
        void $ check config [nefs] [[("d_01-03.jpg", 50)], [("d_02.jpg", 5)]]
                 (0, 0, 4, 0) (30, 55, 0, 0)
      it "doesn't make a movie a composite image (case 6)" $ \config -> do
        dir <- check config [nefs] [[("d_01-03.mov", 70)]] (3, 0, 0, 1) (30, 0, 0, 70)
        roleOf dir "d_01" `shouldBe` PlainImage
        roleOf dir "d_01-03" `shouldBe` PlainImage
      it "uses any component with a master (case 7)" $ \config -> do
        dir <- check config [[("d_02.nef", 10), ("d_03.nef", 10)]] [[("d_01-03.jpg", 50)]]
                 (0, 0, 3, 0) (20, 50, 0, 0)
        roleOf dir "d_01-03" `shouldBe` CompositeOf ["d_02", "d_03"]
        -- The composite keeps only its own files.
        imgRawPath (pdImages dir Map.! "d_01-03") `shouldBe` Nothing
      it "views components with their own jpeg from it (case 8)" $ \config -> do
        dir <- check config [nefs] [[("d_01.jpg", 5), ("d_02.jpg", 5), ("d_03.jpg", 5), ("d_01-03.jpg", 50)]]
                 (0, 0, 4, 0) (30, 65, 0, 0)
        viewedFrom config dir "d_02" `shouldBe` Just "d_02.jpg"
      it "reads an unpadded name as a range (case 9)" $ \config -> do
        dir <- check config [[("x_1.nef", 10), ("x_2.nef", 10), ("x_3.nef", 10)]] [[("x_1-3.jpg", 50)]]
                 (0, 0, 4, 0) (30, 50, 0, 0)
        roleOf dir "x_1-3" `shouldBe` CompositeOf ["x_1", "x_2", "x_3"]
      it "treats a range jpeg in a source directory as a master (case 10)" $ \config ->
        void $ check config [nefs ++ [("d_01-03.jpg", 50)]] [] (1, 0, 3, 0) (80, 0, 0, 0)
      it "links a component to all the composites covering it (case 11)" $ \config -> do
        dir <- check config [nefs] [[("d_01-02.jpg", 50), ("d_02-03.jpg", 40)]]
                 (0, 0, 5, 0) (30, 90, 0, 0)
        roleOf dir "d_02" `shouldBe` ComponentOf ("d_01-02" :| ["d_02-03"])
        roleOf dir "d_01" `shouldBe` ComponentOf ("d_01-02" :| [])
      it "counts a range-named raw as the master (case 12)" $ \config ->
        void $ check config [nefs ++ [("d_01-03.dng", 100)]] [[("d_01-03.jpg", 50)]]
                 (0, 0, 4, 0) (130, 50, 0, 0)
      it "keeps a copy of a range jpeg with the composite (case 13)" $ \config -> do
        dir <- check config [nefs] [[("d_01-03.jpg", 50), ("d_01-03-Edit.jpg", 45)]]
                 (0, 0, 4, 0) (30, 95, 0, 0)
        viewedFrom config dir "d_01" `shouldBe` Just "d_01.nef"
      it "leaves a composite without components standalone (case 14)" $ \config -> do
        dir <- check config [] [[("d_01-03.jpg", 50)]] (0, 1, 0, 0) (0, 0, 50, 0)
        roleOf dir "d_01-03" `shouldBe` CompositeOf []
      it "covers the components of a composite that only has a sidecar" $ \config -> do
        let dir = compositeFolder config [nefs] [[("d_01-03.xmp", 1)]]
        roleOf dir "d_01-03" `shouldBe` CompositeOf ["d_01", "d_02", "d_03"]
        statusOf dir "d_01-03" `shouldBe` ImageOrphaned
        statusOf dir "d_01" `shouldBe` ImageProcessed
      it "dates, and names the camera of, a composite from its first component" $ \config -> do
        let output = def { exifPeople = setFromList [mkSym "Person"]
                         , exifWidth = Just 9000
                         }
            cache = Map.fromList
              [ ("d_02.nef", Right (datedExif 2024 6 2) { exifCamera = Just (mkSym "Cam")
                                                        , exifWidth = Just 6000 })
              , ("d_03.nef", Right (datedExif 2024 6 1))
              , ("d_01-03.jpg", Right output)
              ]
            dir = compositeFolderWith cache config [[("d_02.nef", 10), ("d_03.nef", 10)]]
                    [[("d_01-03.jpg", 50)]]
            exifOf name = imgExif (pdImages dir Map.! name)
            date = LocalTime (fromGregorian 2024 6 2) midnight
        -- d_01 doesn't exist, so the first component is d_02, even if
        -- d_03 is earlier.
        exifLocalCreateDate (exifOf "d_01-03") `shouldBe` Just date
        exifCamera (exifOf "d_01-03") `shouldBe` Just (mkSym "Cam")
        exifWidth (exifOf "d_01-03") `shouldBe` Just 9000
        Set.member (Just date, "d_01-03") (pdTimeSort dir) `shouldBe` True
        -- A component gets nothing from the composite.
        exifPeople (exifOf "d_02") `shouldBe` Set.empty
        exifPeople (exifOf "d_01-03") `shouldBe` setFromList [mkSym "Person"]
      it "gives the same result whatever order the directories are in" $ \config -> do
        let srcs = [nefs ++ [("d_01-03.tif", 100)]]
            outs = [[("d_01-03.jpg", 50)], [("d_02.jpg", 5)]]
            forward = compositeFolder config srcs outs
            backward = finishFolder $
                         foldl' (flip (addDirToRepo config)) Map.empty
                           (reverse $ compositeDirs config srcs outs) Map.! "f"
        -- The images' files can be listed in another order.
        Map.map imgStatus (pdImages forward) `shouldBe` Map.map imgStatus (pdImages backward)
        Map.map imgRole (pdImages forward) `shouldBe` Map.map imgRole (pdImages backward)
        show (pdStats forward) `shouldBe` show (pdStats backward)
      it "makes a composite processed by components with a JPEG master" $ \config -> do
        dir <- check config [[("d_01.jpg", 10), ("d_02.jpg", 10), ("d_03.jpg", 10)]] [[("d_01-03.jpg", 50)]]
                 (0, 0, 4, 0) (30, 50, 0, 0)
        roleOf dir "d_01-03" `shouldBe` CompositeOf ["d_01", "d_02", "d_03"]
      it "doesn't link across sub-directories of the folder" $ \config -> do
        dir <- check config [[("d_01.nef", 10), ("d_02.nef", 10)]] [[("sub/d_01-02.jpg", 50)]]
                 (2, 1, 0, 0) (20, 0, 50, 0)
        roleOf dir "sub/d_01-02" `shouldBe` CompositeOf []
        roleOf dir "d_01" `shouldBe` PlainImage
        dir' <- check config [[("sub/d_01.nef", 10)]] [[("d_01-02.jpg", 50)]]
                  (1, 1, 0, 0) (10, 0, 50, 0)
        roleOf dir' "sub/d_01" `shouldBe` PlainImage
      it "reads a range in a sub-directory of the folder" $ \config -> do
        dir <- check config [[("sub/d_01.nef", 10), ("sub/d_02.nef", 10)]] [[("sub/d_01-02.jpg", 50)]]
                 (0, 0, 3, 0) (20, 50, 0, 0)
        roleOf dir "sub/d_01-02" `shouldBe` CompositeOf ["sub/d_01", "sub/d_02"]
      it "links the names of a wrapped camera counter" $ \config -> do
        dir <- check config [[("d_9999.nef", 10), ("d_0001.nef", 10)]] [[("d_9999-0001.jpg", 50)]]
                 (0, 0, 3, 0) (20, 50, 0, 0)
        roleOf dir "d_9999-0001" `shouldBe` CompositeOf ["d_9999", "d_0001"]
      it "never links a composite image as a component" $ \config' -> do
        -- A range regex under which a composite's name can be in
        -- another one's range: a_1-2-3 has the root a_1- and covers
        -- a_1-2 (itself a composite image) and a_1-3.
        let config = config' { cfgRangeRegex = fromMaybe (error "Invalid range regex") $
                                 mkRegex "^(.*[_-])([0-9]+)-([0-9]+)$" }
            dir = compositeFolder config [[("a_1.nef", 10)]]
                    [[("a_1-2.jpg", 50), ("a_1-2-3.jpg", 50), ("a_1-3.jpg", 5)]]
        roleOf dir "a_1-2-3" `shouldBe` CompositeOf []
        roleOf dir "a_1-2" `shouldBe` CompositeOf ["a_1"]
        roleOf dir "a_1-3" `shouldBe` CompositeOf ["a_1"]
        linksAreConsistent dir
      it "doesn't make components of frames without a master processed" $ \config -> do
        -- Only JPEGs: the composite image, its components and their
        -- statuses stay standalone.
        dir <- check config [] [[("d_01.jpg", 5), ("d_02.jpg", 5), ("d_01-03.jpg", 50)]]
                 (0, 3, 0, 0) (0, 0, 60, 0)
        roleOf dir "d_01-03" `shouldBe` CompositeOf ["d_01", "d_02"]
        forM_ ["d_01", "d_02", "d_01-03"] $ \name ->
          statusOf dir name `shouldBe` ImageStandalone
      it "leaves a component that is only a sidecar orphaned" $ \config -> do
        let dir = compositeFolder config [] [[("d_01-03.jpg", 50), ("d_03.xmp", 1)]]
        roleOf dir "d_03" `shouldBe` ComponentOf ("d_01-03" :| [])
        statusOf dir "d_03" `shouldBe` ImageOrphaned
        statusOf dir "d_01-03" `shouldBe` ImageStandalone
      it "doesn't make a movie a composite image, even with a poster" $ \config -> do
        let dir = compositeFolder config [nefs] [[("d_01-03.mov", 70), ("d_01-03.jpg", 50)]]
        roleOf dir "d_01-03" `shouldBe` PlainImage
        imgRange (pdImages dir Map.! "d_01-03") `shouldBe` Nothing
        statusOf dir "d_01" `shouldBe` ImageUnprocessed
        linksAreConsistent dir
      it "doesn't link a movie as a component" $ \config -> do
        let dir = compositeFolder config [[("d_01.nef", 10), ("d_02.mov", 70)]]
                    [[("d_01-03.jpg", 50)]]
        roleOf dir "d_01-03" `shouldBe` CompositeOf ["d_01"]
        roleOf dir "d_02" `shouldBe` PlainImage
        linksAreConsistent dir

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
