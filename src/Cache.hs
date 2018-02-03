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

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Cache ( cachedBasename
             , writeCacheFile
             , readCacheFile
             ) where

import Types

import Prelude
import qualified Data.ByteString as BS (ByteString, writeFile, readFile)
import qualified Data.ByteString.Lazy as BSL (ByteString, writeFile)
import Data.Time.Clock.POSIX
import System.FilePath (splitFileName)
import System.Directory (createDirectoryIfMissing)
import System.IO.Error
import System.Posix.Files

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
  readContents :: FilePath -> IO a

instance ReadableContent BS.ByteString where
  readContents = BS.readFile

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
  (newestTime `fmap` getFileStatus path) `catchIOError` (\e -> if isDoesNotExistError e ||
                                                                  isPermissionError e
                                                               then return 0
                                                               else ioError e)

pathsSorted :: [FilePath] -> IO Bool
pathsSorted paths = do
  ts <- mapM lastTouch paths
  return $ and . map (\(a, b) -> a <= b) . zip ts $ tail ts

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
               let all_paths = path:extras++[rpath]
               not <$> pathsSorted all_paths
           else return False
  if stale
    then return Nothing
    else Just `fmap` readContents rpath
