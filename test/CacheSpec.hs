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

module CacheSpec (spec) where

import           System.Directory

import           Cache
import           TestImport
import           Types            (Config (..))

failDelete :: Config -> FilePath -> IO ()
failDelete (cfgCacheDir -> cache) path = do
  deleteCacheFile cache path `shouldNotReturn` Nothing

pathShouldExist :: FilePath -> IO ()
pathShouldExist p = doesPathExist p `shouldReturn` True

pathShouldNotExist :: FilePath -> IO ()
pathShouldNotExist p = doesPathExist p `shouldReturn` False

spec :: Spec
spec = parallel $ withConfig $ do
  describe "deleteCachedFile" $ do
    describe "refuses to delete file outside of cache" $ do
      it "refuses to navigate via prefix ../" $ \config -> do
        failDelete config "../raw/x.txt"

      it "refuses to navigate via repeated prefix ../" $ \config -> do
        failDelete config "../../../../../raw/x.txt"

      it "refuses to navigate via deep /../" $ \config -> do
        let cache = cfgCacheDir config
        createDirectoryIfMissing True $ cache </> "test/foo/bar"
        failDelete config "test/../../raw/x.txt"
        failDelete config "test/foo/../../../raw/x.txt"
        failDelete config "test/foo/../foo/bar/../../../../raw/x.txt"

      it "refuses to navigate via relative directory symlinks" $ \config -> do
        let cache = cfgCacheDir config
        createDirectoryLink "../raw" $ cache </> "raw"
        failDelete config "raw/x.txt"

      it "refuses to navigate via deep relative directory symlinks" $ \config -> do
        let cache = cfgCacheDir config
        createDirectoryIfMissing True $ cache </> "test/foo"
        createDirectoryLink "../../../raw" $ cache </> "test/foo/raw"
        failDelete config "test/foo/raw/x.txt"

      it "refuses to navigate via absolute directory symlinks" $ \config -> do
        let cache = cfgCacheDir config
        createDirectoryLink "/etc" $ cache </> "raw"
        failDelete config "raw/passwd"

      it "refuses to navigate via deep absolute directory symlinks" $ \config -> do
        let cache = cfgCacheDir config
            symdir = cache </> "test/foo/bar/raw"
            passwd = symdir </> "passwd"
        createDirectoryIfMissing True $ cache </> "test/foo/bar"
        createDirectoryLink "/etc/" $ symdir
        pathShouldExist passwd
        failDelete config "test/foo/raw/passwd"
        pathShouldExist passwd

    describe "correctly reports failures" $ do
      it "reports delete failure for missing file" $ \config -> do
        failDelete config "x.txt"

      it "reports delete failure for directory" $ \config -> do
        createDirectory $ cfgCacheDir config </> "x"
        failDelete config "x"

    describe "correctly deletes files" $ do
      it "deletes existing file" $ \config -> do
        let cache = cfgCacheDir config
            f = "x.txt"
            fp = cache </> f
        writeFile fp "foobar"
        pathShouldExist fp
        deleteCacheFile cache f `shouldReturn` Nothing
        pathShouldNotExist fp

      it "it deletes valid leaf symlinks" $ \config -> do
        let cache = cfgCacheDir config
            fp = cache </> "x.txt"
        createFileLink "/" $ fp
        pathIsSymbolicLink fp `shouldReturn` True
        pathShouldExist fp
        deleteCacheFile cache "x.txt" `shouldReturn` Nothing
        pathShouldNotExist fp

      it "it deletes stale symlinks" $ \config -> do
        let cache = cfgCacheDir config
            fp = cache </> "x.txt"
        createFileLink "foobar.txt" $ fp
        pathIsSymbolicLink fp `shouldReturn` True
        deleteCacheFile cache "x.txt" `shouldReturn` Nothing
        pathShouldNotExist fp
