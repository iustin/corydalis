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

module Cache ( cachedBasename
             , cacheTempDir
             , resetCacheTempDir
             , CachePathTransformer
             , writeCacheFile
             , readCacheFile
             , readCacheFileSince
             , touchedAt
             , deleteCacheFile
             ) where

import qualified Data.ByteString       as BS (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy  as BSL (ByteString, writeFile)
import           Data.List.NonEmpty    hiding (isPrefixOf, zip)
import           Data.Time.Clock.POSIX
import           System.Directory      (canonicalizePath,
                                        createDirectoryIfMissing,
                                        removeFile, removePathForcibly)
import           System.FilePath       (splitFileName)
import           System.IO.Error
import           System.Posix.Files

import           Import.NoFoundation   hiding (path, tail, toList)
import           Utils.Storage         (isWithinDir)

-- | Builds a cache file path from an original path and a suffix.
cachedBasename :: Config   -- ^ Application configuration (cache directory).
               -> FilePath -- ^ Original file path, used as the cache key.
               -> String   -- ^ Suffix identifying the kind of cache file.
               -> String   -- ^ Cache path: @cachedir/path-suffix@.
cachedBasename config path suffix =
  cfgCacheDir config ++ "/" ++ path ++ "-" ++ suffix

-- | The directory holding the cache files written via a temporary file
-- (the previews, and the images extracted from raw pictures and movies)
-- while they are written, at the root of the cache directory. It also
-- holds the output of the scan's exiftool runs, while they run.
--
-- The cache cleanup doesn't look into it, as it only visits the cache
-- directories of the picture directories (see 'cleanupCache'), so it
-- can't delete a file being written; and as it is emptied at startup,
-- the files left behind by a crash don't accumulate. The files are
-- moved into place by renaming them, so the cache directory must be a
-- single filesystem.
cacheTempDir :: Config -> FilePath
cacheTempDir config = cfgCacheDir config </> ".tmp"

-- | Empties the directory holding the cache files being written,
-- creating it if needed (see 'cacheTempDir').
--
-- This must be done at startup, before anything writes a cache file
-- through it; no other instance may use the same cache directory.
resetCacheTempDir :: Config -> IO ()
resetCacheTempDir config = do
  let dir = cacheTempDir config
  removePathForcibly dir
  createDirectoryIfMissing True dir

-- | Types that can be written as cache file contents.
class WritableContent a where
  writeContents :: FilePath -- ^ Destination path.
                -> a        -- ^ Contents to write.
                -> IO ()

instance WritableContent BS.ByteString where
  writeContents = BS.writeFile

instance WritableContent BSL.ByteString where
  writeContents = BSL.writeFile

-- | Types that can be read from cache files.
class ReadableContent a where
  readContents :: FilePath      -- ^ Path to read.
               -> IO (Maybe a)  -- ^ Contents, or 'Nothing' if missing or unreadable.

instance ReadableContent BS.ByteString where
  readContents p = (Just <$> BS.readFile p) `catchIOError`
                   (\e -> if isDoesNotExistError e ||
                             isPermissionError e
                          then return Nothing
                          else ioError e)

-- | Builds a cache path from the configuration and original path.
--
-- This allows multiple cache files to be written for the same original
-- file, e.g. an exif and a binary exif.
type CachePathTransformer = Config -> FilePath -> FilePath

-- | Writes contents to a cache file, creating parent directories as needed.
writeCacheFile :: (WritableContent a)
               => Config                -- ^ Application configuration.
               -> FilePath              -- ^ Original file path.
               -> CachePathTransformer  -- ^ Cache path transformer.
               -> a                     -- ^ Contents to write.
               -> IO ()
writeCacheFile config path fn contents = do
  let rpath = fn config path
      (parent, _) = splitFileName rpath
  createDirectoryIfMissing True parent
  writeContents rpath contents

-- | Returns the later of a file's modification and status-change times.
newestTime :: FileStatus -- ^ File status as returned by 'getFileStatus'.
           -> POSIXTime  -- ^ Latest of mtime and ctime.
newestTime stat =
  let mtime = modificationTimeHiRes stat
      ctime = statusChangeTimeHiRes stat
  in max mtime ctime

-- | Returns the status of a path, or 'Nothing' if missing or
-- inaccessible (permission denied).
--
-- Uses 'getFileStatus' as we don't recurse into the destination, so if
-- it is a link, that's OK-ish.
statusOf :: FilePath -> IO (Maybe FileStatus)
statusOf path =
  (Just <$> getFileStatus path) `catchIOError` (\e -> if isDoesNotExistError e ||
                                                         isPermissionError e
                                                      then return Nothing
                                                      else ioError e)

-- | Returns the last time a path was touched (the later of its
-- modification and status-change times), or 'Nothing' if missing or
-- inaccessible.
touchedAt :: FilePath -> IO (Maybe POSIXTime)
touchedAt path = fmap newestTime <$> statusOf path

-- | Returns the last time a path was touched, or 0 if missing or
-- inaccessible (see 'touchedAt').
lastTouch :: FilePath     -- ^ Path to stat.
          -> IO POSIXTime -- ^ Latest of mtime and ctime, or 0 if missing
                          -- or inaccessible.
lastTouch path = fromMaybe 0 <$> touchedAt path

-- | Checks whether the given paths have non-decreasing last-touch times.
pathsSorted :: NonEmpty FilePath -- ^ Paths in expected chronological order.
            -> IO Bool           -- ^ 'True' if each path is at least as new as
                                 -- the previous.
pathsSorted paths = do
  ts <- mapM lastTouch paths
  let tpairs = zip (toList ts) (tail ts)
  return $ all (uncurry (<=)) tpairs

-- | Reads a cache file, optionally validating that it is newer than source files.
readCacheFile :: (ReadableContent a)
              => Config                -- ^ Application configuration.
              -> FilePath              -- ^ Original file path.
              -> CachePathTransformer  -- ^ Cache path transformer.
              -> Bool                  -- ^ If 'True', treat the cache
                                       -- as stale when any source is
                                       -- newer. If 'False', always read
                                       -- the cache file if it exists,
                                       -- skipping validation.
              -> [FilePath]            -- ^ Extra source paths that
                                       -- must be older than the cache
                                       -- file.
              -> IO (Maybe a)          -- ^ Cached contents, or
                                       -- 'Nothing' if missing,
                                       -- unreadable, or stale.
readCacheFile config path fn validate extras = do
  let rpath = fn config path
  stale <- if validate
           then do
               let all_paths = path :| (extras++[rpath])
               not <$> pathsSorted all_paths
           else return False
  if stale
    then return Nothing
    else readContents rpath

-- | Reads a cache file, if it is at least as new (see 'touchedAt') as
-- the given time, e.g. that of the file it was built from.
--
-- This is the cheaper form of 'readCacheFile', for reading many small
-- cache files (e.g. the exif caches, during a scan): the time of the
-- file it was built from is given, not looked up.
--
-- Only a regular file is read (reading a pipe would wait for a writer).
-- A missing or unreadable file (permission denied), one that isn't a
-- regular file, and one older than the given time give nothing.
readCacheFileSince :: FilePath   -- ^ Path to the cache file.
                   -> POSIXTime  -- ^ The time it must not be older than.
                   -> IO (Maybe BS.ByteString)
readCacheFileSince path since = do
  stat <- statusOf path
  case stat of
    Just st | isRegularFile st && newestTime st >= since -> readContents path
    _ -> return Nothing

-- | Deletes a cache file, given that it lies under the cache directory.
deleteCacheFile :: FilePath        -- ^ Path to the cache directory.
                -> FilePath        -- ^ Relative path to the cache file.
                -> IO (Maybe Text) -- ^ Error details, or Nothing in case of
                                   -- success.
deleteCacheFile cachedir path = do
  let errfmt :: IOError -> IO (Maybe Text)
      errfmt = return . Just . pack . show
  result <- try $ deleteCacheFile' cachedir path
  either errfmt (const (return Nothing)) result

-- | Inner body for 'deleteCacheFile'.
--
-- It canonicalises the parent of the target, and checks that it (the parent) is
-- still inside the (canonicalised) cache dir. In that case, we can remove the
-- item, since it's either a file under its parent directory, or a symlink to
-- somewhere else (which doesn't matter).
--
-- The cache directory is canonicalised too, as configured it can have a
-- trailing slash or go through symlinks (it is always absolute, as the
-- configuration refuses relative ones); and the paths are compared by whole
-- components, as e.g. @/pics/cache2@ is not within @/pics/cache@.
deleteCacheFile' :: FilePath  -- ^ Path to the cache directory.
                 -> FilePath  -- ^ Relative path to the cache file.
                 -> IO ()
deleteCacheFile' cachedir path = do
  let fp = cachedir </> path
      (parent, leaf) = splitFileName fp
  canonCache <- canonicalizePath cachedir
  canon <- canonicalizePath parent
  if canonCache `isWithinDir` canon
    then removeFile fp
    else ioError $ userError ("Cache path '" ++ path ++ "' resolves to file '" ++ leaf ++
                              "' under '" ++ canon ++
                              "' which does not live under cachedir '" ++ canonCache)
