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
             , CachePathTransformer
             , writeCacheFile
             , readCacheFile
             , deleteCacheFile
             ) where

import qualified Data.ByteString       as BS (ByteString, readFile, writeFile)
import qualified Data.ByteString.Lazy  as BSL (ByteString, writeFile)
import           Data.List.NonEmpty    hiding (isPrefixOf, zip)
import           Data.Time.Clock.POSIX
import           System.Directory      (canonicalizePath,
                                        createDirectoryIfMissing, removeFile)
import           System.FilePath       (splitFileName)
import           System.IO.Error
import           System.Posix.Files

import           Import.NoFoundation   hiding (path, tail, toList)

-- | Builds a cache file path from an original path and a suffix.
cachedBasename :: Config   -- ^ Application configuration (cache directory).
               -> FilePath -- ^ Original file path, used as the cache key.
               -> String   -- ^ Suffix identifying the kind of cache file.
               -> String   -- ^ Cache path: @cachedir/path-suffix@.
cachedBasename config path suffix =
  cfgCacheDir config ++ "/" ++ path ++ "-" ++ suffix

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

-- | Returns the last time a path was touched, or 0 if missing or inaccessible.
--
-- Uses 'getFileStatus' as we don't recurse into the destination, so if
-- it is a link, that's OK-ish.
lastTouch :: FilePath     -- ^ Path to stat.
          -> IO POSIXTime -- ^ Latest of mtime and ctime, or 0 if missing
                          -- or inaccessible.
lastTouch path =
  (newestTime `fmap` getFileStatus path) `catchIOError` (\e -> if isDoesNotExistError e ||
                                                                  isPermissionError e
                                                               then return 0
                                                               else ioError e)

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

-- | Deletes a cache file, given that it lies under the cache directory.
deleteCacheFile :: FilePath        -- ^ Canonical, absolute path to the cache
                                   -- directory.
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
-- still inside the cache dir. In that case, we can remove the item, since it's
-- either a file under its parent directory, or a symlink to somewhere else
-- (which doesn't matter).
deleteCacheFile' :: FilePath  -- ^ Canonical, absolute path to the cache
                              -- directory.
                 -> FilePath  -- ^ Relative path to the cache file.
                 -> IO ()
deleteCacheFile' cachedir path = do
  let fp = cachedir </> path
      (parent, leaf) = splitFileName fp
  canon <- canonicalizePath parent
  if cachedir `isPrefixOf` canon
    then removeFile fp
    else ioError $ userError ("Cache path '" ++ path ++ "' resolves to file '" ++ leaf ++
                              "' under '" ++ canon ++
                              "' which does not live under cachedir '" ++ cachedir)
