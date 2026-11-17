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

module Utils.StorageSpec (spec) where

import           System.Directory   (createDirectoryIfMissing)
import           System.FilePath    (takeDirectory)
import           System.Posix.Files (createSymbolicLink)

import           TestImport
import           Utils.Storage

-- | Sets the configured directories.
withDirs :: [FilePath] -> [FilePath] -> Config -> Config
withDirs src out config =
  config { cfgSourceDirs = src, cfgOutputDirs = out }

-- | Creates an empty file, and its parent directories.
createFile :: FilePath -> IO ()
createFile path = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path ("" :: ByteString)

-- | The (only) source directory.
rawDir :: Config -> FilePath
rawDir = fromMaybe (error "No source directory") . headMay . cfgSourceDirs

spec :: Spec
spec = do
  describe "isWithinDir" $ do
    it "accepts the directory itself" $
      isWithinDir "/pics" "/pics" `shouldBe` True
    it "accepts paths below the directory" $ do
      isWithinDir "/pics" "/pics/2020/a.jpg" `shouldBe` True
      isWithinDir "/pics/" "/pics/2020" `shouldBe` True
    it "compares whole path components" $
      isWithinDir "/pics" "/pics-old/a.jpg" `shouldBe` False
    it "rejects parent and unrelated directories" $ do
      isWithinDir "/pics/2020" "/pics" `shouldBe` False
      isWithinDir "/pics" "/srv/pics" `shouldBe` False
    it "accepts everything within the root directory" $
      isWithinDir "/" "/srv/pics" `shouldBe` True

  describe "storageLocation" $ withConfig $ do
    it "locates source and output paths" $ \config -> do
      let config' = withDirs ["/raw"] ["/jpeg"] config
      storageLocation config' "/raw/2020/trip/a.nef" `shouldBe`
        Just (StorageSource, "/raw", "2020/trip/a.nef")
      storageLocation config' "/jpeg/2020/trip/a.jpg" `shouldBe`
        Just (StorageOutput, "/jpeg", "2020/trip/a.jpg")
    it "rejects paths outside the configured directories" $ \config -> do
      let config' = withDirs ["/raw"] ["/jpeg"] config
      storageLocation config' "/etc/passwd" `shouldBe` Nothing
      storageLocation config' "/raw-old/a.nef" `shouldBe` Nothing
    it "uses the most specific directory" $ \config -> do
      let config' = withDirs ["/pics"] ["/pics/jpeg"] config
      storageLocation config' "/pics/jpeg/2020/a.jpg" `shouldBe`
        Just (StorageOutput, "/pics/jpeg", "2020/a.jpg")
      storageLocation config' "/pics/raw/2020/a.nef" `shouldBe`
        Just (StorageSource, "/pics", "raw/2020/a.nef")

  describe "isCanonicalStoragePath" $ withConfig $ do
    it "accepts a regular file" $ \config -> do
      let path = rawDir config </> "2020" </> "trip" </> "a.nef"
      createFile path
      isCanonicalStoragePath config path `shouldReturn` True
    it "accepts a missing file" $ \config ->
      isCanonicalStoragePath config (rawDir config </> "missing.nef")
        `shouldReturn` True
    it "rejects a file outside the configured directories" $ \config -> do
      let path = cfgCacheDir config </> "a.nef"
      createFile path
      isCanonicalStoragePath config path `shouldReturn` False
    it "rejects a symbolic link" $ \config -> do
      let target = rawDir config </> "a.nef"
          path = rawDir config </> "b.nef"
      createFile target
      createSymbolicLink target path
      isCanonicalStoragePath config path `shouldReturn` False
    it "rejects a file under a symbolic link to a directory" $ \config -> do
      let dir = rawDir config </> "2020"
      createFile (dir </> "trip" </> "a.nef")
      createSymbolicLink (dir </> "trip") (dir </> "link")
      isCanonicalStoragePath config (dir </> "link" </> "a.nef")
        `shouldReturn` False
    it "accepts a configured directory which is a symbolic link" $ \config -> do
      let rawLink = takeDirectory (rawDir config) </> "rawlink"
          config' = withDirs [rawLink] [] config
      createFile (rawDir config </> "a.nef")
      createSymbolicLink (rawDir config) rawLink
      isCanonicalStoragePath config' (rawLink </> "a.nef") `shouldReturn` True
