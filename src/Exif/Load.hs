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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Loading the exif data of a folder's files: from the exif caches,
-- or by running exiftool (which then writes the caches).
module Exif.Load ( getExif
#ifdef TEST
                 , exifToolBatchSize
                 , chunkPaths
                 , exifToolArgs
                 , exifPath
                 , bExifPath
#endif
                 ) where

import qualified Data.Aeson
import           Data.Aeson                (eitherDecodeStrict')
import           Data.Bifunctor
import qualified Data.ByteString           as BS (ByteString, length, readFile)
import qualified Data.ByteString.Lazy      as BSL (null)
import qualified Data.Map.Strict           as Map
import           Data.Store
import qualified Data.Text                 as Text
import           Data.Time.Clock.POSIX     (POSIXTime)
import           System.Directory          (createDirectoryIfMissing)
import           System.Log.FastLogger     (toLogStr)
import           System.Process.Typed

import           Cache
import           Exif
import           Import.NoFoundation       hiding (path)
import           Utils.Tools               (decodeToolOutput, runToolToFile,
                                            withToolSlot)

-- TODO: make this saner/ensure it's canonical path.
buildPath :: FilePath -> FilePath -> FilePath
buildPath dir name = dir ++ "/" ++ name

-- | Writes the (binary) exif cache for a given file.
writeBExif :: Config -> FilePath -> EExif -> IO ()
writeBExif config path =
  writeCacheFile config path bExifPath . Data.Store.encode

-- | Tries to read the (binary) exif cache for a given file, given the
-- file's last change (see 'touchedAt').
--
-- The cache is used only if it is at least as new as the raw exif cache,
-- itself at least as new as the file; without the raw exif cache (missing
-- or unreadable), it isn't used.
readBExif :: Config -> POSIXTime -> FilePath -> IO (Maybe EExif)
readBExif config fileTime path = do
  rawTime <- touchedAt (exifPath config path)
  contents <- case rawTime of
                Just t | fileTime <= t -> readCacheFileSince (bExifPath config path) t
                _                      -> return Nothing
  return $ contents >>= either (const Nothing) Just . Data.Store.decode

-- | Writes the exif caches (raw and binary) for a given file.
writeExifs :: Config -> FilePath -> ERawExif -> IO (FilePath, EExif)
writeExifs config dir er = do
  let rpath = either freFilePath (Text.unpack . rExifSrcFile) er
      fpath = buildPath dir rpath
      ro    = either (fmap Data.Aeson.encode . freValue)
                     (Just . Data.Aeson.encode . rExifRaw) er
      e     = bimap freMessage (exifFromRaw config) er
  maybe (return ()) (writeCacheFile config fpath exifPath) ro
  writeBExif config fpath e
  return (rpath, e)

-- | Tries to read the (raw) exif cache for a given file, given the
-- file's last change (see 'touchedAt'): the cache is used only if it is
-- at least as new.
readExif :: Config -> POSIXTime -> FilePath -> IO (Maybe (Either Text RawExif))
readExif config fileTime path = do
  contents <- readCacheFileSince (exifPath config path) fileTime
  return $ contents >>= Just . first Text.pack . eitherDecodeStrict'

-- | Insert an exif value after fully evaluating it so cache/JSON thunks
-- (especially a RawExif Object) can be garbage-collected.
--
-- The map is evaluated as well, as it would otherwise be a chain of
-- pending insertions, each holding its key unevaluated (an evaluated
-- map has evaluated keys).
insertForced :: Map Text EExif -> Text -> EExif -> IO (Map Text EExif)
insertForced m k e = do
  e' <- evaluate $ force e
  return $! Map.insert k e' m

-- | Try to get an exif value for a path, either from cache or from filesystem.
--
-- The files are given (relative to the directory) with their last
-- change, the later of their modification and status change times (see
-- 'touchedAt'), as the scan found them: the caches older than that are
-- not used.
getExif :: LogFn
        -> ToolLimits  -- ^ The limits of the scan's exiftool runs.
        -> Config -> FilePath -> [(FilePath, POSIXTime)]
        -> IO (Int, Map Text EExif)
getExif logfn tools config dir files = do
  logfn LevelDebug $ "getExif " <> toLogStr dir <> ": " <>
    toLogStr (show (length files)) <> " paths"
  (cache1, m1) <- foldM (\(c, m) f@(p, ftime) -> do
                            let fpath = buildPath dir p
                            exif <- readBExif config ftime fpath
                            case exif of
                              Nothing -> return (c, f:m)
                              Just e  -> do
                                c' <- insertForced c (Text.pack p) e
                                return (c', m)
                        ) (Map.empty, []) files
  logfn LevelDebug $ "getExif " <> toLogStr dir <> ": bexif hits=" <>
    toLogStr (show (Map.size cache1)) <> " misses=" <>
    toLogStr (show (length m1))
  (cache2, m2) <- foldM (\(c, m) (p, ftime) -> do
                            let fpath = buildPath dir p
                            exif <- readExif config ftime fpath
                            case exif of
                              -- missing, unreadable, not a regular file, or stale.
                              Nothing -> return (c, p:m)
                              -- found file, but parsing might have failed.
                              Just v -> do
                                e <- evaluate $ force $ second (exifFromRaw config) v
                                writeBExif config fpath e
                                c' <- insertForced c (Text.pack p) e
                                return (c', m)
                 ) (cache1, []) m1
  logfn LevelDebug $ "getExif " <> toLogStr dir <> ": json hits=" <>
    toLogStr (show (Map.size cache2 - Map.size cache1)) <> " exiftool=" <>
    toLogStr (show (length m2))
  cache3 <- foldM (runExifToolBatch logfn tools config dir) cache2
              (chunkPaths m2)
  return (length m2, cache3)

-- | Max files per exiftool invocation.
--
-- @exiftool -json -l@ emits a @{num,val}@ object per tag, so a single
-- RAW can be 100KB+ of JSON. Parsing a whole folder at once retains
-- the ByteString, the Aeson tree, and a RawExif Object per file, which
-- OOMs even for a few thousand files.
exifToolBatchSize :: Int
exifToolBatchSize = 32

chunkPaths :: [a] -> [[a]]
chunkPaths [] = []
chunkPaths xs =
  let (h, t) = splitAt exifToolBatchSize xs
  in h : chunkPaths t

-- | Runs exiftool on a batch of files, within the given limits, and
-- adds their exif data to the given map.
--
-- If the run fails (e.g. exiftool can't be started, is stopped for
-- running too long, or its output can't be parsed), all the files of
-- the batch get its error. They aren't retried one by one, which could
-- take as long again for each file. The error isn't cached either, as
-- it isn't about the files themselves, so the next scan runs exiftool
-- on them again (and, if the same file makes it fail again, fails the
-- whole batch again).
runExifToolBatch :: LogFn -> ToolLimits -> Config -> FilePath -> Map Text EExif
                 -> [FilePath] -> IO (Map Text EExif)
runExifToolBatch logfn tools config dir acc batch = do
  let failed msg = do
        logfn LevelError $ "exiftool batch failed in " <> toLogStr dir <>
          ": " <> toLogStr msg
        return $ Left msg
  -- TODO: fix this. It is very ugly, catches _all_
  -- exceptions, but it's the only way I found to
  -- reliably disable the slowloris protection. There a
  -- quite a few issues on the wai project regarding the
  -- timeout reaper, but without conclusive
  -- solutions. See
  -- https://github.com/yesodweb/wai/issues/351 for
  -- example.
  exifs <- (extractExifs logfn tools config dir batch >>=
              either failed (return . parseExifs)) `catch`
    (\e -> failed $ sformat shown (e :: SomeException))
  case exifs of
    -- The batch's error isn't written to the exif caches: it isn't
    -- about the files themselves, which the next scan tries again.
    Left msg -> do
      -- Evaluated, as it can hold the exception it was built from.
      msg' <- evaluate $ force msg
      foldM (\m p -> insertForced m (Text.pack p) (Left msg')) acc batch
    Right parsed -> do
      logfn LevelDebug $ "extractExifs: parsed " <> toLogStr (show (length parsed)) <>
        " results for " <> toLogStr dir
      foldM (\m r -> do
               (path, e) <- writeExifs config dir r
               insertForced m (Text.pack path) e
            ) acc parsed

exifPath :: Config -> FilePath -> FilePath
exifPath config path =
  cachedBasename config path "exif"

bExifPath :: Config -> FilePath -> FilePath
bExifPath config path =
  cachedBasename config path "bexif"

-- | Builds the exiftool command line for paths relative to its working
-- directory.
--
-- The paths come from the file system, and a name starting with a dash
-- would otherwise be parsed as an option: @-Comment=x.jpg@ switches
-- exiftool to write mode, modifying the other files in the batch. The
-- paths must therefore always come after the end of options marker.
exifToolArgs :: [FilePath] -> [String]
exifToolArgs paths = [
  "-json",
  "-struct",
  "-l",
  "--"
  ] ++ paths

-- | Runs exiftool on the given files, in a slot of the given limits,
-- returning its (JSON) output, or why it failed (e.g. it couldn't be
-- run, or ran for too long).
--
-- The slot is taken first, so that the runs waiting for one don't hold
-- a temporary file. The output goes to that file, rather than being
-- kept in memory while exiftool runs. The file is in the cache's
-- temporary directory (see 'cacheTempDir'), as the other temporary
-- files: it is removed afterwards, and, if the application is killed
-- meanwhile, at the next start. It is named after the end of the
-- directory's path (its last 50 characters, at most 200 bytes, with
-- slashes replaced), which shows which folder is being read, while
-- leaving room for the rest of the name: the whole path could make the
-- name too long.
extractExifs :: LogFn -> ToolLimits -> Config -> FilePath -> [FilePath]
             -> IO (Either Text BS.ByteString)
extractExifs logfn tools config dir paths =
  withToolSlot tools $ \slot -> do
    let tmpDir = cacheTempDir config
    createDirectoryIfMissing True tmpDir
    withTempFile tmpDir (tmpName dir) $ \fpath fhandle -> do
      hClose fhandle
      let args = exifToolArgs paths
          pconfig =
            setStdin closed
            . setCloseFds True
            . setWorkingDir dir
            $ proc "exiftool" args
      logfn LevelDebug $ "extractExifs: spawning exiftool on " <>
        toLogStr (show (length paths)) <> " files in " <> toLogStr dir <>
        " -> " <> toLogStr fpath
      ran <- runToolToFile slot fpath pconfig
      case ran of
        Left msg -> return $ Left msg
        -- Note: exiftool exits with non-zero exit code if errors happened,
        -- but still generates valid JSON output. So we ignore the exit code
        -- completely, as we can't attribute the error to a specific
        -- file. That should be rather done via 'Error' entry in the object.
        Right (_, err) -> do
          unless (BSL.null err) . logfn LevelDebug $
            "extractExifs: exiftool messages for " <> toLogStr dir <> ": " <>
            toLogStr (decodeToolOutput err)
          logfn LevelDebug $ "extractExifs: exiftool exited, reading " <> toLogStr fpath
          bs <- BS.readFile fpath
          logfn LevelDebug $ "extractExifs: read " <> toLogStr (show (BS.length bs)) <>
            " bytes"
          return $ Right bs
  where tmpName = (++ "-exif.json") . reverse . take 50 . reverse
                  . map (\c -> if c == '/' then '_' else c)
