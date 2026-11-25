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

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- | Locating paths within the configured picture directories.
module Utils.Storage ( StorageKind(..)
                     , isWithinDir
                     , storageLocation
                     , isCanonicalStoragePath
                     , ResolvedStorageDirs
                     , resolveStorageDirs
                     , isCanonicalStoragePathIn
                     , storageKindName
                     , relativeStoragePath
                     , UnsafeDir(..)
                     , privateDirsInStorage
                     , checkPrivateDirsOutsideStorage
                     ) where

import           Data.Either         (fromRight)
import qualified Data.Map.Strict     as Map
import           System.Directory    (canonicalizePath)
import           System.Log.FastLogger (toLogStr)
import           System.FilePath     (makeRelative, normalise,
                                      splitDirectories, takeFileName)

import           Import.NoFoundation

-- | The kind of a configured picture directory.
data StorageKind = StorageSource  -- ^ A source (e.g. raw) directory.
                 | StorageOutput  -- ^ An output (processed) directory.
                 deriving (Eq, Show)

-- | A short name of the kind of a picture directory, for display.
storageKindName :: StorageKind -> Text
storageKindName StorageSource = "source"
storageKindName StorageOutput = "output"

-- | Shows a path without the location of the configured directories:
-- the path relative to its configured directory, prefixed by its kind
-- (but not by which directory, if there are several of that kind).
--
-- Paths outside the configured directories are shown as just their
-- file name.
relativeStoragePath :: Config -> FilePath -> Text
relativeStoragePath config fpath =
  case storageLocation config fpath of
    Just (kind, _, rel) -> storageKindName kind <> ": " <> pack rel
    Nothing             -> pack $ takeFileName fpath

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
isCanonicalStoragePath config =
  checkCanonicalStoragePath config (fmap Just . canonicalizePath)

-- | The configured picture directories, resolved (see
-- 'resolveStorageDirs'), for checking many paths in one pass.
--
-- Each directory maps to its resolved path, or to nothing if resolving
-- it failed.
newtype ResolvedStorageDirs = ResolvedStorageDirs (Map FilePath (Maybe FilePath))

-- | Resolves the configured picture directories, for checking many
-- paths (see 'isCanonicalStoragePathIn'), e.g. all the pictures, after
-- a scan.
--
-- The result must only be used for one such pass, not kept: it doesn't
-- see later changes of the configured directories (e.g. one of them
-- replaced by a symbolic link to elsewhere). A directory which can't be
-- resolved (e.g. permission denied) fails the checks of all its paths;
-- the reason is logged here, once, as the failed checks can't tell it.
resolveStorageDirs :: LogFn -> Config -> IO ResolvedStorageDirs
resolveStorageDirs logfn config =
  ResolvedStorageDirs . Map.fromList <$>
    mapM resolve (cfgSourceDirs config ++ cfgOutputDirs config)
  where
    resolve dir = tryIO (canonicalizePath dir) >>= \case
      Right cdir -> return (dir, Just cdir)
      Left err -> do
        logfn LevelError $ "Cannot resolve the picture directory " <>
          toLogStr dir <> ": " <> toLogStr (show err)
        return (dir, Nothing)

-- | Checks a path as 'isCanonicalStoragePath' does, but with the
-- configured directories resolved beforehand (see
-- 'resolveStorageDirs'), so that checking many paths resolves each
-- directory only once. The path itself is still resolved.
isCanonicalStoragePathIn :: ResolvedStorageDirs -> Config -> FilePath -> IO Bool
isCanonicalStoragePathIn (ResolvedStorageDirs dirs) config =
  checkCanonicalStoragePath config (return . join . (`Map.lookup` dirs))

-- | Checks a path as 'isCanonicalStoragePath' describes, resolving its
-- configured directory with the given function (nothing failing the
-- check).
checkCanonicalStoragePath :: Config -> (FilePath -> IO (Maybe FilePath))
                          -> FilePath -> IO Bool
checkCanonicalStoragePath config resolveDir fpath =
  case storageLocation config fpath of
    Nothing -> return False
    -- Links are allowed only in the configured directory, so resolve
    -- just that, and append the relative part as is: if resolving the
    -- whole path gives something else, the relative part has a link (or
    -- a "..").
    Just (_, dir, rel) -> do
      result <- tryIO $ do
        mcdir <- resolveDir dir
        case mcdir of
          Nothing -> return False
          Just cdir -> do
            cpath <- canonicalizePath fpath
            return $ pathComponents cpath == pathComponents (cdir </> rel)
      return $ fromRight False result

-- | A directory which must not hold private files, and why.
data UnsafeDir = PictureDir  -- ^ Its files are listed, and possibly shown.
               | CacheDir    -- ^ Its files can be deleted.
               deriving (Eq, Show)

-- | Finds the private directories which are, or are within, one of
-- the unsafe directories.
--
-- Returns the private directories, with the unsafe directories holding
-- them. The paths should be canonical.
privateDirsInStorage :: [(UnsafeDir, FilePath)]  -- ^ The unsafe directories.
                     -> [FilePath]               -- ^ The private directories.
                     -> [(FilePath, (UnsafeDir, FilePath))]
privateDirsInStorage unsafeDirs privateDirs =
  [ (private, unsafe)
  | private <- privateDirs
  , unsafe@(_, dir) <- unsafeDirs
  , isWithinDir dir private
  ]

-- | Checks that the private directories (holding e.g. the session key,
-- or the database) are not within the configured picture directories,
-- where their files could be listed (and possibly served), nor within
-- the cache directory, where they could be deleted, by the cache
-- cleanup or by someone clearing the cache.
--
-- Returns an error message otherwise. The directories are compared
-- after resolving them, so symbolic links, or relative paths, don't
-- hide an overlap.
checkPrivateDirsOutsideStorage :: Config -> [FilePath] -> IO (Either Text ())
checkPrivateDirsOutsideStorage config privateDirs = do
  unsafeDirs <- mapM (traverse canonicalizePath) $
                  map (PictureDir,) (cfgSourceDirs config ++ cfgOutputDirs config)
                  ++ [(CacheDir, cfgCacheDir config)]
  privateDirs' <- mapM canonicalizePath privateDirs
  return $ case privateDirsInStorage unsafeDirs privateDirs' of
    [] -> Right ()
    overlaps -> Left . intercalate "; " $ map describe overlaps
  where
    describe (private, (kind, dir)) =
      sformat ("The directory " % string % " holds private files, but is \
               \within the " % stext % " " % string % ", " % stext)
        private (kindName kind) dir (consequence kind)
    kindName PictureDir = "picture directory"
    kindName CacheDir   = "cache directory"
    consequence PictureDir = "so these files could be shown"
    consequence CacheDir   =
      "whose contents can be deleted (by Corydalis, or when clearing the cache)"
