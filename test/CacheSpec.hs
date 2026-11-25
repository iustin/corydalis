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

import qualified Data.ByteString.Char8 as BS8
import           Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import           System.Directory
import           System.FilePath    (takeDirectory)
import           System.Posix.Files (createNamedPipe, setFileTimesHiRes)

import           Cache
import           TestImport

failDelete :: Config -> FilePath -> IO ()
failDelete (cfgCacheDir -> cache) path = do
  deleteCacheFile cache path `shouldNotReturn` Nothing

pathShouldExist :: FilePath -> IO ()
pathShouldExist p = doesPathExist p `shouldReturn` True

pathShouldNotExist :: FilePath -> IO ()
pathShouldNotExist p = doesPathExist p `shouldReturn` False

-- | Writes a small file, creating its directory if needed.
writeSome :: FilePath -> IO ()
writeSome path = do
  createDirectoryIfMissing True (takeDirectory path)
  BS8.writeFile path "x"

setMtimeOffset :: FilePath -> POSIXTime -> IO ()
setMtimeOffset path delta = do
  now <- getPOSIXTime
  let stamp = now + delta
  setFileTimesHiRes path stamp stamp

spec :: Spec
spec = parallel $ withConfig $ do
  describe "cachedBasename" $ do
    it "builds cache basename from cache dir, path, and suffix" $ \config -> do
      cachedBasename config "foo/bar" "thumb"
        `shouldBe` (cfgCacheDir config ++ "/foo/bar-thumb")

  describe "writeCacheFile/readCacheFile" $ do
    let sourceRoot config = fromMaybe (error "No source directory in test config")
                                      (headMay $ cfgSourceDirs config)
        sourcePath config = sourceRoot config </> "image.nef"
        extraPath config = sourceRoot config </> "image.xmp"
        cachePath config = cfgCacheDir config </> "nested/cache.bin"
        cacheFn config _ = cachePath config
        cacheData = BS8.pack "cache-data"

    it "writes cache files and creates missing parent directories" $ \config -> do
      writeCacheFile config (sourcePath config) cacheFn cacheData
      pathShouldExist (cachePath config)
      BS8.readFile (cachePath config) `shouldReturn` cacheData

    it "returns Nothing for missing cache files" $ \config -> do
      readCacheFile config (sourcePath config) cacheFn False []
        `shouldReturn` (Nothing :: Maybe BS8.ByteString)

    it "reads cache when validation is disabled even if source becomes newer" $ \config -> do
      writeFile (sourcePath config) "raw"
      writeCacheFile config (sourcePath config) cacheFn cacheData
      setMtimeOffset (sourcePath config) 3600
      readCacheFile config (sourcePath config) cacheFn False []
        `shouldReturn` Just cacheData

    it "reads cache when source and extras are not newer than cache" $ \config -> do
      writeFile (sourcePath config) "raw"
      writeFile (extraPath config) "xmp"
      writeCacheFile config (sourcePath config) cacheFn cacheData
      readCacheFile config (sourcePath config) cacheFn True [extraPath config]
        `shouldReturn` Just cacheData

    it "returns Nothing when source is newer than cache and validation is enabled" $ \config -> do
      writeFile (sourcePath config) "raw"
      writeCacheFile config (sourcePath config) cacheFn cacheData
      setMtimeOffset (sourcePath config) 3600
      readCacheFile config (sourcePath config) cacheFn True []
        `shouldReturn` (Nothing :: Maybe BS8.ByteString)

    it "returns Nothing when an extra dependency is newer than cache" $ \config -> do
      writeFile (sourcePath config) "raw"
      writeFile (extraPath config) "xmp"
      writeCacheFile config (sourcePath config) cacheFn cacheData
      setMtimeOffset (extraPath config) 3600
      readCacheFile config (sourcePath config) cacheFn True [extraPath config]
        `shouldReturn` (Nothing :: Maybe BS8.ByteString)

  describe "readCacheFileSince" $ do
    let cachePath config = cfgCacheDir config </> "nested/cache.bin"
        cacheData = BS8.pack "cache-data"
        writeCache config = do
          createDirectoryIfMissing True (takeDirectory (cachePath config))
          BS8.writeFile (cachePath config) cacheData
        -- With a last resort time limit, in case the read blocks.
        readSince path since =
          timeout (30 * 1000000) (readCacheFileSince path since)

    it "reads a cache file at least as new as the given time" $ \config -> do
      writeCache config
      now <- getPOSIXTime
      readSince (cachePath config) (now - 3600) `shouldReturn` Just (Just cacheData)
      readSince (cachePath config) 0 `shouldReturn` Just (Just cacheData)

    it "doesn't read a cache file older than the given time" $ \config -> do
      writeCache config
      now <- getPOSIXTime
      readSince (cachePath config) (now + 3600) `shouldReturn` Just Nothing

    it "returns Nothing for a missing cache file" $ \config ->
      readSince (cachePath config) 0 `shouldReturn` Just Nothing

    it "reads an empty cache file as empty" $ \config -> do
      createDirectoryIfMissing True (takeDirectory (cachePath config))
      BS8.writeFile (cachePath config) ""
      readSince (cachePath config) 0 `shouldReturn` Just (Just "")

    it "doesn't read a directory" $ \config -> do
      createDirectoryIfMissing True (cachePath config)
      readSince (cachePath config) 0 `shouldReturn` Just Nothing

    it "doesn't read a pipe, without waiting for a writer" $ \config -> do
      createDirectoryIfMissing True (takeDirectory (cachePath config))
      createNamedPipe (cachePath config) 0o600
      readSince (cachePath config) 0 `shouldReturn` Just Nothing

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
        createDirectoryLink "/etc/" symdir
        pathShouldExist passwd
        failDelete config "test/foo/raw/passwd"
        pathShouldExist passwd

      it "refuses a sibling directory starting with the cache's name" $ \config -> do
        let cache = cfgCacheDir config
            sibling = cache ++ "2"
        createDirectory sibling
        writeFile (sibling </> "x.txt") ("foobar" :: ByteString)
        createDirectoryLink sibling $ cache </> "c2"
        failDelete config "c2/x.txt"
        pathShouldExist $ sibling </> "x.txt"

    describe "accepts other spellings of the cache directory" $ do
      let deleteVia dir config = do
            let fp = cfgCacheDir config </> "x.txt"
            writeFile fp ("foobar" :: ByteString)
            deleteCacheFile dir "x.txt" `shouldReturn` Nothing
            pathShouldNotExist fp

      it "with a trailing slash" $ \config ->
        deleteVia (cfgCacheDir config ++ "/") config

      it "through a symlink" $ \config -> do
        let cache = cfgCacheDir config
            cacheLink = takeDirectory cache </> "cache-link"
        createDirectoryLink cache cacheLink
        deleteVia cacheLink config

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
        createFileLink "/" fp
        pathIsSymbolicLink fp `shouldReturn` True
        pathShouldExist fp
        deleteCacheFile cache "x.txt" `shouldReturn` Nothing
        pathShouldNotExist fp

      it "it deletes stale symlinks" $ \config -> do
        let cache = cfgCacheDir config
            fp = cache </> "x.txt"
        createFileLink "foobar.txt" fp
        pathIsSymbolicLink fp `shouldReturn` True
        deleteCacheFile cache "x.txt" `shouldReturn` Nothing
        pathShouldNotExist fp

  describe "resetCacheTempDir" $ do
    it "creates the cache's temporary directory" $ \config -> do
      resetCacheTempDir config
      listDirectory (cacheTempDir config) `shouldReturn` []
    it "empties it, leaving the rest of the cache alone" $ \config -> do
      let tmpDir = cacheTempDir config
          cached = cfgCacheDir config </> "pics" </> "a.jpg-1024"
      createDirectoryIfMissing True (tmpDir </> "sub")
      writeSome (tmpDir </> "a.jpg-1024123.tmp")
      writeSome (tmpDir </> "sub" </> "file")
      writeSome cached
      resetCacheTempDir config
      listDirectory tmpDir `shouldReturn` []
      doesFileExist cached `shouldReturn` True
    it "doesn't follow symbolic links in it" $ \config -> do
      let tmpDir = cacheTempDir config
          outside = cfgCacheDir config </> "outside"
      writeSome outside
      createDirectoryIfMissing True tmpDir
      createFileLink outside (tmpDir </> "link")
      resetCacheTempDir config
      listDirectory tmpDir `shouldReturn` []
      pathShouldExist outside
