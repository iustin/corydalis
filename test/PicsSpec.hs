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

module PicsSpec (spec) where

import           Data.Default
import qualified Data.Map     as Map

import           Pics
import           TestImport

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
  withContext $
    describe "search cache" $ do
      it "caches a search result" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
        getSearchResults ctx m1 [] `shouldReturn` m1
        getSearchResults ctx (error "Failed to cache") [] `shouldReturn` m1
      it "flushes the search cache on rescan" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
            m2 = (Map.empty, Map.empty)
        launchScanFileSystem ctx
        _ <- waitForScan ctx
        getSearchResults ctx m1 [] `shouldReturn` m1
        getSearchResults ctx m2 [] `shouldReturn` m1
        launchScanFileSystem ctx
        _ <- waitForScan ctx
        getSearchResults ctx m2 [] `shouldReturn` m2
