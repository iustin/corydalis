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
             ) where

import Types

import Prelude
import qualified Data.ByteString as BS (ByteString, writeFile)
import qualified Data.ByteString.Lazy as BSL (ByteString, writeFile)
import System.FilePath (splitFileName)
import System.Directory (createDirectoryIfMissing)

cachedBasename :: Config -> FilePath -> String -> String
cachedBasename config path suffix =
  cfgCacheDir config ++ "/" ++ path ++ "-" ++ suffix

class WritableContent a where
  writeContents :: FilePath -> a -> IO ()

instance WritableContent BS.ByteString where
  writeContents = BS.writeFile

instance WritableContent BSL.ByteString where
  writeContents = BSL.writeFile

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
