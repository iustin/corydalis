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

cachedBasename :: Config -> FilePath -> String -> String
cachedBasename config path suffix =
  cfgCacheDir config ++ "/" ++ path ++ "-" ++ suffix

class WritableContent a where
  writeContents :: FilePath -> a -> IO ()

instance WritableContent BS.ByteString where
  writeContents = BS.writeFile

instance WritableContent BSL.ByteString where
  writeContents = BSL.writeFile

class ReadableContent a where
  readContents :: FilePath -> IO (Maybe a)

instance ReadableContent BS.ByteString where
  readContents p = (Just <$> BS.readFile p) `catchIOError`
                   (\e -> if isDoesNotExistError e ||
                             isPermissionError e
                          then return Nothing
                          else ioError e)

writeCacheFile :: (WritableContent a) =>
                  Config
               -> FilePath
               -> (Config -> FilePath -> FilePath)
               -> a
               -> IO ()
writeCacheFile config path fn contents = do
  let rpath = fn config path
      (parent, _) = splitFileName rpath
  createDirectoryIfMissing True parent
  writeContents rpath contents

newestTime :: FileStatus -> POSIXTime
newestTime stat =
  let mtime = modificationTimeHiRes stat
      ctime = statusChangeTimeHiRes stat
  in max mtime ctime

lastTouch :: FilePath -> IO POSIXTime
lastTouch path =
  -- Note: this uses getFileStatus as we don't recurse into the
  -- destination, so if it is a link, that's OK-ish.
  (newestTime `fmap` getFileStatus path) `catchIOError` (\e -> if isDoesNotExistError e ||
                                                                  isPermissionError e
                                                               then return 0
                                                               else ioError e)

pathsSorted :: NonEmpty FilePath -> IO Bool
pathsSorted paths = do
  ts <- mapM lastTouch paths
  let tpairs = zip (toList ts) (tail ts)
  return $ all (uncurry (<=)) tpairs

readCacheFile :: (ReadableContent a) =>
                 Config
              -> FilePath
              -> (Config -> FilePath -> FilePath)
              -> Bool
              -> [FilePath]
              -> IO (Maybe a)
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
      errfmt err = return . Just . pack . show $ err
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
