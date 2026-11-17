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
{-# LANGUAGE TupleSections     #-}

-- | Locating paths within the configured picture directories.
module Utils.Storage ( StorageKind(..)
                     , isWithinDir
                     , storageLocation
                     , isCanonicalStoragePath
                     ) where

import           Data.Either         (fromRight)
import           System.Directory    (canonicalizePath)
import           System.FilePath     (makeRelative, normalise,
                                      splitDirectories)

import           Import.NoFoundation

-- | The kind of a configured picture directory.
data StorageKind = StorageSource  -- ^ A source (e.g. raw) directory.
                 | StorageOutput  -- ^ An output (processed) directory.
                 deriving (Eq, Show)

-- | Whether a path is a directory, or is below it.
--
-- This compares whole path components, so e.g. @/pics-old@ is not
-- within @/pics@; the paths are not resolved, so they should be
-- either canonical, or built from the same base.
isWithinDir :: FilePath  -- ^ The directory.
            -> FilePath  -- ^ The path to check.
            -> Bool
isWithinDir dir fpath =
  pathComponents dir `isPrefixOf` pathComponents fpath

-- | The components of a path, after normalisation.
pathComponents :: FilePath -> [FilePath]
pathComponents = splitDirectories . normalise

-- | Locates a path within the configured picture directories.
--
-- Returns the kind and the path of the (most specific) configured
-- directory holding the path, and the path relative to it, or nothing
-- if the path is not within any of them.
storageLocation :: Config -> FilePath
                -> Maybe (StorageKind, FilePath, FilePath)
storageLocation config fpath =
  case fromNullable (filter (\(_, dir) -> isWithinDir dir fpath) roots) of
    Nothing -> Nothing
    Just found ->
      let (kind, dir) = maximumBy (comparing depth) found
      in Just (kind, dir, makeRelative dir fpath)
  where roots = map (StorageSource,) (cfgSourceDirs config) ++
                map (StorageOutput,) (cfgOutputDirs config)
        depth = length . pathComponents . snd

-- | Checks that a path is within the configured picture directories,
-- and that no part of it below the configured directory is a symbolic
-- link.
--
-- The configured directories themselves can be (or be under) symbolic
-- links. Errors (e.g. permission denied) result in a failed check.
isCanonicalStoragePath :: Config -> FilePath -> IO Bool
isCanonicalStoragePath config fpath =
  case storageLocation config fpath of
    Nothing -> return False
    -- Links are allowed only in the configured directory, so resolve
    -- just that, and append the relative part as is: if resolving the
    -- whole path gives something else, the relative part has a link (or
    -- a "..").
    Just (_, dir, rel) -> do
      result <- tryIO $ do
        cdir <- canonicalizePath dir
        cpath <- canonicalizePath fpath
        return $ pathComponents cpath == pathComponents (cdir </> rel)
      return $ fromRight False result
