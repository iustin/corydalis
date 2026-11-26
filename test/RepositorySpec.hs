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

module RepositorySpec (spec) where

import           Data.Default
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Data.Time (LocalTime (..), ZonedTime (..), midnight,
                            zonedTimeToLocalTime)

import           AtomTypes
import           Exif
import           Repository
import           TestImport

-- | A file for which exiftool reported the given MIME type.
fileWithMimeType :: Maybe Text -> File
fileWithMimeType mime =
  (simpleFile "a.jpg") { fileExif = def { exifMimeType = mkSym <$> mime } }

spec :: Spec
spec = parallel $ do
  describe "inode info" $ do
    it "works without directories" $ \_ -> do
      let ii = InodeInfo "file.jpg" [] False 0 0 0
      inodeFullName ii `shouldBe` "file.jpg"
    it "returns the full path for an inode" $ \_ -> do
      let ii = InodeInfo "file.jpg" ["subdir", "dir"] False 0 0 0
      inodeFullName ii `shouldBe` "dir/subdir/file.jpg"

  describe "file info" $ do
    it "empty parents return the file name" $ \_ -> do
      let f = File { fileName = "file.jpg", fileCTime = 0, fileMTime = 0, fileSize = 0, fileParent = mkSym "", fileDirs = mkSym "", fileExif = def }
      fileFullPath f `shouldBe` "file.jpg"
      fileRelPath f `shouldBe` "file.jpg"
    it "returns the full path for a file" $ \_ -> do
      let f = File { fileName = "file.jpg", fileCTime = 0, fileMTime = 0, fileSize = 0, fileDirs = mkSym "", fileParent = mkSym "/pics/2022", fileExif = def }
      fileFullPath f `shouldBe` "/pics/2022/file.jpg"
      fileRelPath f `shouldBe` "file.jpg"
    it "returns the full path for a file with dirs" $ \_ -> do
      let f = File { fileName = "file.jpg", fileCTime = 0, fileMTime = 0, fileSize = 0, fileDirs = mkSym "dir/subdir", fileParent = mkSym "/pics/2022", fileExif = def }
      fileFullPath f `shouldBe` "/pics/2022/dir/subdir/file.jpg"
      fileRelPath f `shouldBe` "dir/subdir/file.jpg"

  describe "file MIME type" $ do
    forM_ ["image/jpeg", "image/png", "image/avif", "image/x-canon-cr2",
           "video/mp4", "video/quicktime"] $ \mime ->
      it ("uses the type reported by exiftool for " ++ show mime) $ \_ ->
        fileMimeType "image/jpeg" (fileWithMimeType (Just mime)) `shouldBe` mime
    -- Types that browsers treat as documents, malformed types, and header
    -- injection attempts all get the default.
    forM_ ["image/svg+xml", "IMAGE/SVG+XML", "text/html", "text/plain",
           "application/xhtml+xml", "application/pdf", "text/xml", "",
           "image/jpeg; charset=utf-8", "image/jpeg\r\nX-Injected: 1"] $ \mime ->
      it ("uses the default instead of " ++ show mime) $ \_ ->
        fileMimeType "image/jpeg" (fileWithMimeType (Just mime)) `shouldBe` "image/jpeg"
    it "uses the default without a reported type" $ \_ ->
      fileMimeType "video/mp4" (fileWithMimeType Nothing) `shouldBe` "video/mp4"

  describe "computeRepoStats" $ do
    it "returns empty event stats for an empty repository" $ \_ -> do
      rsEventStats (computeRepoStats Map.empty) `shouldBe` Map.empty
    it "counts folders by event kind" $ \_ -> do
      let noEvent = createTestPicDir "none"
          birthday = (createTestPicDir "bday")
           { pdEvent = Just BirthdayEvent { eventName = "x", eventPeople = [], eventSource = EventExplicit Nothing } }
          generic1 = (createTestPicDir "g1")
           { pdEvent = Just GenericEvent { eventName = "a", eventPeople = [], eventSource = EventExplicit Nothing } }
          generic2 = (createTestPicDir "g2")
           { pdEvent = Just GenericEvent { eventName = "b", eventPeople = [], eventSource = EventExplicit Nothing } }
          dirs = Map.fromList [(pdName d, d) | d <- [noEvent, birthday, generic1, generic2]]
      rsEventStats (computeRepoStats dirs) `shouldBe` Map.fromList
        [ (EKNoEvent, 1)
        , (EKBirthday, 1)
        , (EKGeneric, 2)
        ]

  withConfig $ do
    describe "isHiddenComponent and imagesShownFor" $ do
      it "hide a component without a JPEG of its own" $ \config -> do
        let dir = compositeFolder config
                    [[("d_01.nef", 10), ("d_02.nef", 10), ("d_03.nef", 10), ("x.nef", 10)]]
                    [[("d_01-03.jpg", 50), ("d_02.jpg", 5)]]
            img n = pdImages dir Map.! n
            shown n = map imgName (imagesShownFor dir (img n))
        map (isHiddenComponent . img) ["d_01", "d_02", "d_03", "d_01-03", "x"]
          `shouldBe` [True, False, True, False, False]
        shown "d_01" `shouldBe` ["d_01-03"]
        shown "d_02" `shouldBe` ["d_02"]
        shown "d_01-03" `shouldBe` ["d_01-03"]
        shown "x" `shouldBe` ["x"]
    describe "NFData" $ do
      it "forces the media type" $ \_ ->
        evaluate (rnf (error "type" :: MediaType)) `shouldThrow` anyErrorCall
      it "forces image movie and untracked files" $ \config -> do
        let img = simpleRawImage config
        evaluate (rnf img) `shouldReturn` ()
        evaluate (rnf (img { imgMasterMov = Just (error "master-mov") }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (img { imgMovs = [error "mov"] }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (img { imgUntracked = [error "untracked"] }))
          `shouldThrow` anyErrorCall
      it "forces promoted image exif" $ \config -> do
        let img = simpleRawImage config
            thunked = img { imgExif = def { exifTitle = Just (error "title") } }
        evaluate (rnf thunked) `shouldThrow` anyErrorCall
      it "forces folder time sort, stats date range, and events" $ \_ -> do
        let dir = createTestPicDir "test"
        evaluate (rnf dir) `shouldReturn` ()
        evaluate (rnf (dir { pdTimeSort = Set.singleton (Just (error "time"), "a") }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (dir { pdEvent = Just (error "event") }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (dir { pdStats = def { sDateRange = Just (error "range") } }))
          `shouldThrow` anyErrorCall

    describe "mkImageStatus" $ do
      it "classifies backing-file combinations" $ \config -> do
        let raw = simpleFile "a.nef"
            jpeg = simpleFile "a.jpg"
            sidecar = simpleFile "a.xmp"
        mkImageStatus config (Just raw) [] Nothing False `shouldBe` ImageUnprocessed
        mkImageStatus config (Just raw) [jpeg] Nothing False `shouldBe` ImageProcessed
        mkImageStatus config Nothing [jpeg] Nothing False `shouldBe` ImageStandalone
        mkImageStatus config Nothing [] (Just sidecar) False `shouldBe` ImageOrphaned
        mkImageStatus config Nothing [jpeg] (Just sidecar) False `shouldBe` ImageStandalone
        mkImageStatus config Nothing [] Nothing True `shouldBe` ImageProcessed
      it "errors when nothing backs the image" $ \config ->
        evaluate (mkImageStatus config Nothing [] Nothing False) `shouldThrow` anyErrorCall

    describe "stats and folder class" $ do
      it "counts sizes and files across image kinds" $ \config -> do
        let raw = mkImage config "a" "t" (Just $ fileSized "a.nef" 100) Nothing [] Nothing [] [] Nothing MediaImage def
            standalone = mkImage config "b" "t" Nothing Nothing [fileSized "b.jpg" 40] Nothing [] [] Nothing MediaImage def
            processed = mkImage config "c" "t" (Just $ fileSized "c.nef" 50) Nothing [fileSized "c.jpg" 20] Nothing [] [] Nothing MediaImage def
            orphaned = mkImage config "d" "t" Nothing (Just $ fileSized "d.xmp" 5) [] Nothing [] [] Nothing MediaImage def
            movie = mkImage config "e" "t" Nothing Nothing [] (Just $ fileSized "e.mov" 80) [] [] Nothing MediaMovie def
            untracked = mkImage config "f" "t" Nothing Nothing [] Nothing [] [fileSized "f.other" 7] Nothing MediaUnknown def
            stats = foldl' updateStatsWithPic zeroStats [raw, standalone, processed, orphaned, movie, untracked]
        sRaw stats `shouldBe` 1
        sStandalone stats `shouldBe` 1
        sProcessed stats `shouldBe` 1
        sOrphaned stats `shouldBe` 1
        sMovies stats `shouldBe` 1
        sUntracked stats `shouldBe` 1
        totalStatsCount stats `shouldBe` 6
        totalStatsSize stats `shouldBe` 100 + 40 + 50 + 20 + 5 + 80 + 7
        sRawSize stats `shouldBe` 150
      it "sums two stats structures" $ \_ -> do
        let a = zeroStats { sRaw = 1, sRawSize = 10 }
            b = zeroStats { sStandalone = 2, sStandaloneSize = 5 }
            s = sumStats a b
        sRaw s `shouldBe` 1
        sStandalone s `shouldBe` 2
        totalStatsSize s `shouldBe` 15
      it "maps stats onto folder classes" $ \_ -> do
        folderClassFromStats zeroStats `shouldBe` FolderEmpty
        folderClassFromStats (zeroStats { sRaw = 1 }) `shouldBe` FolderRaw
        folderClassFromStats (zeroStats { sRaw = 1, sProcessed = 1 }) `shouldBe` FolderUnprocessed
        folderClassFromStats (zeroStats { sStandalone = 1 }) `shouldBe` FolderStandalone
        folderClassFromStats (zeroStats { sStandalone = 1, sProcessed = 1 }) `shouldBe` FolderMixed
        folderClassFromStats (zeroStats { sOrphaned = 1 }) `shouldBe` FolderMixed
        folderClassFromStats (zeroStats { sProcessed = 1 }) `shouldBe` FolderProcessed
        folderClassFromStats (zeroStats { sMovies = 1 }) `shouldBe` FolderProcessed
      it "computes folderClass from images" $ \config -> do
        folderClass (createTestPicDir "empty") `shouldBe` FolderEmpty
        folderClass ((createTestPicDir "empty") { pdUntracked = [simpleFile "SHA1SUMS"] })
          `shouldBe` FolderEmpty
        folderClass (picDirWith "raw" [simpleRawImage config]) `shouldBe` FolderRaw

    describe "queries" $ do
      it "filters images by class" $ \config -> do
        let raw = simpleRawImage config
            jpeg = mkImage config "b" "test" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def
            dir = picDirWith "test" [raw, jpeg]
            repo = mkRepository (Map.singleton "test" dir)
        map imgName (filterImagesByClass [ImageUnprocessed] repo) `shouldBe` ["a"]
        map imgName (filterImagesByClass [ImageStandalone] repo) `shouldBe` ["b"]
      it "omits folder-level untracked files from allRepoFiles" $ \config -> do
        let dir = (picDirWith "test" [simpleRawImage config])
                    { pdUntracked = [simpleFile "SHA1SUMS"] }
            repo = mkRepository (Map.singleton "test" dir)
        map fileName (allRepoFiles repo) `shouldBe` ["a.nef"]
      it "reports image file kinds" $ \config -> do
        let raw = simpleRawImage config
            movie = mkImage config "m" "t" Nothing Nothing [] (Just $ simpleFile "m.mov") [] [] Nothing MediaMovie def
            untracked = simpleUntrackedImage config "t" "u"
        imageHasImages raw `shouldBe` True
        imageHasMovies raw `shouldBe` False
        imageHasMovies movie `shouldBe` True
        imageHasUntracked untracked `shouldBe` True
        imageHasUntracked raw `shouldBe` False
        null (allImageFiles raw) `shouldBe` False
        null (allViewableImageFiles raw) `shouldBe` False
      it "counts pictures in a folder" $ \config -> do
        let dir = picDirWith "test" [simpleRawImage config]
        numPics dir `shouldBe` 1
        numRawPics dir `shouldBe` 1
        hasViewablePics dir `shouldBe` False
        hasViewablePics (picDirWith "s" [mkImage config "b" "s" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def]) `shouldBe` True
      it "aggregates image problems" $ \config -> do
        let warned = (simpleRawImage config) { imgExif = def { exifWarning = Set.singleton "bad" } }
            clean = mkImage config "b" "test" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def
            dir = picDirWith "test" [warned, clean]
        imgProblems warned `shouldBe` Set.singleton "exif: bad"
        pdProblems dir Map.! Just "exif: bad" `shouldBe` 1
        pdProblems dir Map.! Nothing `shouldBe` 1
      it "builds time keys from exif dates" $ \config -> do
        let img = (simpleRawImage config) { imgExif = datedExif 2020 7 15 }
        imageYear img `shouldBe` Just 2020
        imageYearMonth img `shouldBe` Just (2020, 7)
        fst (imageTimeKey img) `shouldBe` Just (LocalTime (fromGregorian 2020 7 15) midnight)

  withContext $ do
    describe "countImagesBy" $ do
      it "counts images by attribute, the most frequent first" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = [ simpleUntrackedImage config f n
                   | (f, n) <- [("a", "1"), ("b", "1"), ("b", "2"), ("c", "1")
                               , ("c", "2"), ("c", "3")] ]
        countImagesBy (Just . imgParent) imgs `shouldBe`
          [(3, "c"), (2, "b"), (1, "a")]
      it "orders equal counts by descending attribute" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = map (simpleUntrackedImage config "f") ["x", "y", "z"]
        countImagesBy (Just . imgName) imgs `shouldBe`
          [(1, "z"), (1, "y"), (1, "x")]
      it "skips images without the attribute" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = map (simpleUntrackedImage config "f") ["x", "y"]
            attr i = if imgName i == "x" then Nothing else Just (imgName i)
        countImagesBy attr imgs `shouldBe` [(1, "y")]
        countImagesBy (const (Nothing :: Maybe Int)) imgs `shouldBe` []

    describe "imageDateRange" $ do
      let dated config name date =
            exifImage config "f" name (\e -> e { exifCreateDate = date })
          day = localDay . zonedTimeToLocalTime . etTime
          dates = fmap (\((d1, a1), (d2, a2)) -> (day d1, a1, day d2, a2))
      it "finds the earliest and the latest dated images" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = [ dated config "b" (Just (exifDate 2021 1 1))
                   , dated config "a" (Just (exifDate 2019 1 1))
                   , dated config "n" Nothing
                   , dated config "c" (Just (exifDate 2020 1 1))
                   ]
        dates (imageDateRange imgName imgs) `shouldBe`
          Just (fromGregorian 2019 1 1, "a", fromGregorian 2021 1 1, "b")
      it "orders images with the same date by attribute" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = [ dated config n (Just (exifDate 2021 1 1)) | n <- ["y", "x", "z"] ]
        fmap (\((_, a1), (_, a2)) -> (a1, a2)) (imageDateRange imgName imgs)
          `shouldBe` Just ("x", "z")
      it "is the same image for a single dated image" $ \ctx -> do
        let imgs = [dated (ctxConfig ctx) "x" (Just (exifDate 2021 1 1))]
        fmap (\((_, a1), (_, a2)) -> (a1, a2)) (imageDateRange imgName imgs)
          `shouldBe` Just ("x", "x")
      it "is nothing without dated images" $ \ctx -> do
        let imgs = [dated (ctxConfig ctx) "x" Nothing]
        dates (imageDateRange imgName imgs) `shouldBe` Nothing
        dates (imageDateRange imgName []) `shouldBe` Nothing

    describe "imageNeighbours" $ do
      let results config =
            let mk (f, n) = simpleUntrackedImage config f n
                imgs = map mk [("a", "1"), ("b", "1"), ("b", "2"), ("b", "3"), ("c", "1")]
                fldrs = map mk [("a", "1"), ("b", "1"), ("c", "1")]
            in ( Map.fromList [((imgParent i, imageTimeKey i), i) | i <- imgs]
               , Map.fromList [(imgParent i, i) | i <- fldrs])
          names = fmap (\i -> (imgParent i, imgName i))
          namesOf n = ( names (Just (nbFirst n)), names (nbPrevFolder n)
                      , names (nbPrev n), names (nbNext n)
                      , names (nbNextFolder n), names (Just (nbLast n)))
      it "finds all neighbours of an image in the middle" $ \ctx -> do
        let config = ctxConfig ctx
            img = simpleUntrackedImage config "b" "2"
        namesOf (imageNeighbours (results config) img) `shouldBe`
          ( Just ("a", "1"), Just ("a", "1"), Just ("b", "1"), Just ("b", "3")
          , Just ("c", "1"), Just ("c", "1"))
      it "crosses folder boundaries for the previous and next images" $ \ctx -> do
        let config = ctxConfig ctx
            n = imageNeighbours (results config) (simpleUntrackedImage config "b" "1")
            n' = imageNeighbours (results config) (simpleUntrackedImage config "b" "3")
        names (nbPrev n) `shouldBe` Just ("a", "1")
        names (nbNext n') `shouldBe` Just ("c", "1")
      it "has no previous items for the first image" $ \ctx -> do
        let config = ctxConfig ctx
            img = simpleUntrackedImage config "a" "1"
        namesOf (imageNeighbours (results config) img) `shouldBe`
          ( Just ("a", "1"), Nothing, Nothing, Just ("b", "1")
          , Just ("b", "1"), Just ("c", "1"))
      it "has no next items for the last image" $ \ctx -> do
        let config = ctxConfig ctx
            img = simpleUntrackedImage config "c" "1"
        namesOf (imageNeighbours (results config) img) `shouldBe`
          ( Just ("a", "1"), Just ("b", "1"), Just ("b", "3"), Nothing
          , Nothing, Just ("c", "1"))
      it "orders the images of a folder by date, then by name" $ \ctx -> do
        let config = ctxConfig ctx
            dated name y = exifImage config "d" name (\e -> e { exifCreateDate = fmap (\y' -> exifDate y' 1 1) y })
            -- Names in the opposite order of the dates; undated first.
            imgs = [dated "1" (Just 2021), dated "2" (Just 2020), dated "3" Nothing]
            res = (Map.fromList [((imgParent i, imageTimeKey i), i) | i <- imgs], Map.empty)
            n = imageNeighbours res (dated "2" (Just 2020))
        names (nbPrev n) `shouldBe` Just ("d", "3")
        names (nbNext n) `shouldBe` Just ("d", "1")
        names (Just (nbFirst n)) `shouldBe` Just ("d", "3")
        names (Just (nbLast n)) `shouldBe` Just ("d", "1")
      it "finds the neighbours of an image missing from the results" $ \ctx -> do
        let config = ctxConfig ctx
            n = imageNeighbours (results config) (simpleUntrackedImage config "b" "15")
        namesOf n `shouldBe`
          ( Just ("a", "1"), Just ("a", "1"), Just ("b", "1"), Just ("b", "2")
          , Just ("c", "1"), Just ("c", "1"))
      it "uses the image itself as first and last for a single result" $ \ctx -> do
        let img = simpleUntrackedImage (ctxConfig ctx) "a" "1"
            single = ( Map.singleton (imgParent img, imageTimeKey img) img
                     , Map.singleton (imgParent img) img)
        imageNeighbours single img `shouldBe`
          ImageNeighbours img Nothing Nothing Nothing Nothing img
      it "uses the image itself as first and last for empty results" $ \ctx -> do
        let img = simpleUntrackedImage (ctxConfig ctx) "a" "1"
        imageNeighbours (Map.empty, Map.empty) img `shouldBe`
          ImageNeighbours img Nothing Nothing Nothing Nothing img
