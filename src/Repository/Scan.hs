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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Scanning the source and output trees into a repository, then
-- rendering the previews and cleaning up the preview cache; and
-- writing, reading and removing the repository cache.
module Repository.Scan
  ( waitForScan
  , launchScanFileSystem
  , scanAll
#ifdef TEST
  , isOKDir
  , makeRel
  , getDirContents
  , scanBaseDir
  , countDir
  , recursiveScanPath
  , folderYamlFile
  , loadFolder
  , isLiveCacheFile
  , examineCacheFile
  , runScanAction
  , renderableImages
  , forceBuildThumbCaches
  , repoCachePath
  , writeDiskCache
#endif
  ) where

import           Control.Applicative
import           Control.DeepSeq
import qualified Data.Map.Strict         as Map
import           Data.Semigroup
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Short         as TS
import           Data.Time.LocalTime
import           Debug.Trace.String
import           System.Directory        hiding (isSymbolicLink)
import           System.FilePath
import           System.Log.FastLogger   (toLogStr)
import           System.Mem              (performMajorGC)
import qualified System.Posix.Files      (fileSize)
import           System.Posix.Files      hiding (fileSize)
import           System.Posix.Directory  (closeDirStream, openDirStream)
import           System.Posix.Directory.Internals
                                         (dirEntName, dirEntType,
                                          getRealDirType, isDirectoryType,
                                          readDirStreamWith)
import           System.Posix.Internals  (peekFilePath)
import qualified Text.Regex.TDFA         as TDFA
import           UnliftIO.Exception

import           Cache
import           Compat.Orphans          ()
import           Exif
import           Import.NoFoundation     hiding (fileName, fileSize, path)
import           Utils.Storage           (resolveStorageDirs)
import           Utils.Yaml              (loadYaml, maxYamlSize)

import           Repository
import           Repository.Build
import           Repository.Encoding     (decodeRepository, encodeRepository)
import           Repository.State
import           Pics

neverRecurseDirs :: [String]
neverRecurseDirs = [".", ".."]

blacklistedDirs :: Config -> [String]
blacklistedDirs config = neverRecurseDirs ++ cfgBlacklistedDirs config

isOKDir :: Config -> String -> Bool
isOKDir cfg = TDFA.match (reRegex $ cfgDirRegex cfg)

-- | Makes a path "absolute" by dropping prefix slashes.
makeRel :: FilePath -> FilePath
makeRel = dropWhile (== pathSeparator)

-- | The reason a directory entry is skipped, if it is neither a
-- directory nor a regular file.
--
-- Symbolic links are not followed, as they could point outside the
-- configured directories, and e.g. make any file readable by the
-- application available for download; other special files (pipes,
-- sockets, devices) are not pictures, and reading them could block.
skipReason :: FileStatus -> Maybe Text
skipReason stat
  | isSymbolicLink stat = Just "symbolic link, not followed"
  | isDirectory stat || isRegularFile stat = Nothing
  | otherwise = Just "not a regular file or directory, ignored"

-- | Lists a directory, returning the sub-directories, the regular
-- files, and the skipped entries (see 'skipReason') with the reason.
getDirContents :: Config -> FilePath
               -> IO ([FilePath], [InodeInfo], [(FilePath, Text)])
getDirContents config root = do
  contents <- getDirectoryContents root
  let allowed_names = filter (`notElem` blacklistedDirs config) contents
  (paths, skipped) <-
    foldM (\(acc, skip) path -> do
             stat <- getSymbolicLinkStatus $ root </> path
             case skipReason stat of
               Just reason -> return (acc, (path, reason):skip)
               Nothing -> do
                 let !ii = InodeInfo { inodeName  = path
                                     , inodeDirs  = []
                                     , inodeIsDir = isDirectory stat
                                     , inodeMTime = modificationTimeHiRes stat
                                     , inodeCTime = statusChangeTimeHiRes stat
                                     , inodeSize  = System.Posix.Files.fileSize stat
                                     }
                 ii' <- evaluate $!! ii
                 return (ii':acc, skip)
       ) ([], []) allowed_names
  let (dirs, files) = partition inodeIsDir paths
  return $!! (map inodeName dirs, files, skipped)

-- | Records the entries skipped while scanning as scan errors.
reportSkipped :: Ctx
              -> FilePath            -- ^ The directory holding the entries.
              -> [(FilePath, Text)]  -- ^ The skipped entries and reasons.
              -> IO ()
reportSkipped _ _ [] = return ()
reportSkipped ctx root skipped = do
  let errs = skippedErrors root skipped
  mapM_ (\(ProgressError item reason) ->
           ctxLogger ctx LevelWarn $ "Skipping " <> toLogStr item <>
             ": " <> toLogStr reason) errs
  atomically $ modifyTVar' (ctxScanProgress ctx) (incProgress errs 0 0)

-- | Converts skipped entries to scan errors.
skippedErrors :: FilePath -> [(FilePath, Text)] -> [ProgressError]
skippedErrors root =
  map (\(path, reason) -> ProgressError (Text.pack $ root </> path) reason)

-- | How a directory is handled when looking for picture folders.
data DirKind = PictureFolder  -- ^ A picture folder, loaded whole.
             | ContainerDir   -- ^ A directory searched for picture folders.
             | TooDeepDir     -- ^ A directory not searched, as too deep.
               deriving (Eq)

-- | Classifies a directory found while looking for picture folders,
-- given its depth below the source or output directory (1 for that
-- directory's own sub-directories).
--
-- A directory whose name matches the directory regex is a picture
-- folder; any other directory is searched for picture folders, unless
-- at the maximum folder depth.
dirKind :: Config -> Int -> FilePath -> DirKind
dirKind config depth name
  | isOKDir config name = PictureFolder
  | depth < cfgMaxFolderDepth config = ContainerDir
  | otherwise = TooDeepDir

-- | The reason a directory is not searched for picture folders, if it
-- is too deep (see 'dirKind').
tooDeepReason :: Config -> Text
tooDeepReason config =
  "not a picture folder, and at the maximum folder depth (" <>
  tshow (cfgMaxFolderDepth config) <> "), so not searched"

-- | Scans one of the directories defined in the configuration.
scanBaseDir :: Ctx
            -> FilePath
            -> Bool
            -> IO [PicDir]
scanBaseDir ctx = scanContainer ctx 0

-- | Scans a directory for picture folders, at any depth down to the
-- maximum folder depth (see 'dirKind'). The names of the directories
-- above the picture folders are discarded and will not appear in the
-- final folder names; files directly in them are ignored.
scanContainer :: Ctx
              -> Int       -- ^ The directory's depth below the base dir.
              -> FilePath  -- ^ The directory to scan.
              -> Bool      -- ^ Whether it is under a source directory.
              -> IO [PicDir]
scanContainer ctx depth path isSource = do
  let config = ctxConfig ctx
      kind = dirKind config (depth + 1)
      ofKind k = filter ((== k) . kind)
  (dirs, _, skipped) <- getDirContents config path
  reportSkipped ctx path $
    skipped ++ map (, tooDeepReason config) (ofKind TooDeepDir dirs)
  -- Picture folders are loaded one at a time, but concurrently with
  -- the other directories.
  (folders, subdirs) <- concurrently
    (mapM (\d -> loadFolder ctx d (path </> d) isSource)
       (ofKind PictureFolder dirs))
    (mapConcurrently (\d -> scanContainer ctx (depth + 1) (path </> d) isSource)
       (ofKind ContainerDir dirs))
  return $ folders ++ concat subdirs

-- | Counts the items under a source or output directory, as
-- 'scanBaseDir' would scan them: the files in picture folders, and
-- the entries reported as scan errors at all levels.
countDir :: Config -> FilePath -> IO Int
countDir config = countContainer 0
  where
    countContainer depth path = do
      (dirs, _, skipped) <- getDirContents config path
      subdirs <- mapConcurrently (\d -> case dirKind config (depth + 1) d of
                                     PictureFolder -> countFolder (path </> d)
                                     ContainerDir  -> countContainer (depth + 1) (path </> d)
                                     TooDeepDir    -> return 1
                                 ) dirs
      return $!! sum subdirs + length skipped
    countFolder path = do
      (dirs, files, skipped) <- getDirContents config path
      subdirs <- mapConcurrently (countFolder . (path </>)) dirs
      return $!! sum subdirs + length files + length skipped

-- | Lists a directory, returning the names of its sub-directories and
-- those of its other entries, without @.@ and @..@.
--
-- Only the names and types the directory itself holds are read, with
-- no status call per entry, unless the file system doesn't give the
-- type (then the entry's own status is read, and an entry removed
-- meanwhile is left out). Symbolic links are not followed: a link to a
-- directory is an entry like a file.
--
-- The directory is read whole and closed before returning, so that a
-- walk of a tree doesn't keep one open per level.
listDirTyped :: FilePath -> IO ([FilePath], [FilePath])
listDirTyped dir =
  bracket (openDirStream dir) closeDirStream (readAll [] [])
  where
    readAll !dirs !others stream = do
      next <- readDirStreamWith entry stream
      case next of
        Nothing                    -> return (dirs, others)
        Just Nothing               -> readAll dirs others stream
        Just (Just (name, True))   -> readAll (name:dirs) others stream
        Just (Just (name, False))  -> readAll dirs (name:others) stream
    entry ent = do
      name <- dirEntName ent >>= peekFilePath
      if name == "." || name == ".."
        then return Nothing
        else do
          kind <- (Just <$> (dirEntType ent >>=
                     getRealDirType (getSymbolicLinkStatus (dir </> name))))
                    `catchIO` \e -> if isDoesNotExistError e
                                       then return Nothing
                                       else throwIO e
          return $!! (\k -> (name, isDirectoryType k)) <$> kind

-- | Runs an action on all entries under a directory (given relative to
-- the root), except the directories themselves, which are walked into
-- (see 'listDirTyped'): regular files, and also symbolic links and
-- other special files.
--
-- The action gets each entry's path relative to the root.
allPathFiles :: FilePath -> (FilePath -> IO ()) -> FilePath -> IO ()
allPathFiles root handler (makeRel -> subdir) = do
  let buildsub p = if subdir == "." then p else subdir </> p
  (dirs, others) <- listDirTyped (root </> subdir)
  mapM_ (handler . buildsub) others
  mapM_ (allPathFiles root handler . buildsub) dirs

-- | Builds the filepath and filestatus pairs recursively for all
-- entries beneats a directory, and returns the skipped entries (with
-- the path relative to the directory) separately.
recursiveScanPath :: Config
                  -> FilePath
                  -> [FilePath]
                  -> IO ([InodeInfo], [(FilePath, Text)])
recursiveScanPath config root stack = do
  (!dirs, !files, !skipped) <- getDirContents config root
  let with_prefix = map (\ii -> ii { inodeDirs = stack }) files
      relpath p = System.FilePath.joinPath . reverse $ p:stack
      skipped' = map (first relpath) skipped
  subdirs <- mapM (\p -> recursiveScanPath config (root </> p)
                           (p:stack)) dirs
  return (with_prefix ++ concatMap fst subdirs,
          skipped' ++ concatMap snd subdirs)

-- | The name of the per-folder YAML file.
folderYamlName :: FilePath
folderYamlName = "corydalis.yaml"

-- | The per-folder YAML file to load, if any, among the folder's files
-- found by the scan: its name, or the reason not to load it.
--
-- As the scan only lists regular files (see 'skipReason'), the file is
-- not loaded if it is a symbolic link or a special file, which are
-- reported as for pictures; and since it is read whole, it is not
-- loaded if larger than 'maxYamlSize' (as found by the scan).
folderYamlFile :: [InodeInfo] -> Maybe (Either Text FilePath)
folderYamlFile contents = do
  ii <- find (\i -> null (inodeDirs i) && inodeName i == folderYamlName) contents
  return $ if toInteger (inodeSize ii) > toInteger maxYamlSize
             then Left $ "larger than " <> tshow maxYamlSize <> " bytes"
             else Right (inodeName ii)

-- | Builds a `PicDir` from an entire filesystem subtree.
loadFolder :: Ctx       -- ^ Scan context.
           -> String    -- ^ Folder name.
           -> FilePath  -- ^ Folder path on disk.
           -> Bool      -- ^ Whether the folder is under a source directory.
           -> IO PicDir
loadFolder ctx name path isSource = do
  let config = ctxConfig ctx
      scanProgress = ctxScanProgress ctx
      logfn = ctxLogger ctx
  logfn LevelDebug $ "loadFolder scan " <> toLogStr path
  (contents, skipped) <- recursiveScanPath config path []
  reportSkipped ctx path skipped
  logfn LevelDebug $ "loadFolder scanned " <> toLogStr path <> " (" <>
    toLogStr (show (length contents)) <> " inodes)"
  let known = filter (isKnownMediaInode config) contents
  (readexifs, lcache) <- getExif logfn (ctxScanToolLimits ctx) config path $
                           map (\i -> (inodeFullName i, inodeMTime i `max` inodeCTime i))
                               known
  logfn LevelDebug $ "loadFolder yaml " <> toLogStr path
  let warnYaml reason = logfn LevelWarn $ "Folder " <> toLogStr path <>
        ": " <> toLogStr folderYamlName <> " ignored: " <> toLogStr reason
  yamlEvent <- case folderYamlFile contents of
    Nothing            -> return Nothing
    Just (Left reason) -> Nothing <$ warnYaml reason
    Just (Right fname) ->
      loadYaml (path </> fname) >>=
        either (\e -> Nothing <$ warnYaml (pack e :: Text)) (return . Just)
  logfn LevelDebug $ "loadFolder build " <> toLogStr path
  let totalitems = length contents
      noopexifs = max (totalitems - readexifs) 0
      folder = buildFolderFromInodes config name path isSource contents lcache yamlEvent
  logfn LevelDebug $ "loadFolder progress " <> toLogStr path
  atomically $ modifyTVar' scanProgress (incProgress [] noopexifs readexifs)
  logfn LevelDebug $ "loadFolder force " <> toLogStr path
  folder' <- evaluate $ force folder
  logfn LevelDebug $ "loadFolder done " <> toLogStr path
  return folder'

-- | The number of cache files expected for each file in the repository,
-- for the estimated goal of the cache cleanup (see 'cleanupCache'): the
-- two exif caches, and a preview per automatically built size.
--
-- It is only an estimate: not every file has previews (e.g. sidecars),
-- while some have more cache files (e.g. the embedded image of a raw
-- file, or previews built on demand).
expectedCacheFiles :: Config -> Int
expectedCacheFiles config = 2 + Set.size (cfgAutoImageSizes config)

-- | Whether a cache file is in use: its path is exactly that of a file
-- in the repository, followed by an ending in use (see
-- 'cacheFileEndings').
isLiveCacheFile :: [Text]    -- ^ The endings in use ('cacheFileEndings')
                -> Set Text  -- ^ The files in the repository (absolute paths)
                -> Text      -- ^ The cache file's path, without the cache
                             -- directory (so starting with a separator)
                -> Bool
isLiveCacheFile endings files path =
  any (\ending -> maybe False (`Set.member` files) $
                    Text.stripSuffix ending path) endings

-- | Checks if a given cache file is obsolete, and deletes it if so.
--
-- Only the cache files in use are kept (see 'isLiveCacheFile'); all the
-- others are deleted, e.g. those of files no longer in the repository,
-- those of sizes no longer configured, and the temporary files that
-- earlier development versions left next to the cache files when
-- interrupted. The cache files being written are in 'cacheTempDir',
-- which isn't cleaned up here.
examineCacheFile :: FilePath       -- ^ Absolute prefix path on the filesystem
                 -> TVar Progress  -- ^ For tracking progress
                 -> [Text]         -- ^ Endings of the cache files in use
                 -> Set Text       -- ^ Set of real image files (absolute paths)
                 -> LogFn          -- ^ Logger function
                 -> FilePath       -- ^ Relative path to the item being examined
                 -> IO ()
examineCacheFile prefix cleanProgress endings iset logger path = do
  let found = isLiveCacheFile endings iset (Text.pack $ pathSeparator:path)
  modifier <- if found
              then return incNoop
              else cleanCacheFile prefix logger path
  atomically $ modifyTVar' cleanProgress (raiseGoal . modifier)
  where
    -- The goal is only an estimate (see 'cleanupCache'); it is kept
    -- above the files examined so far.
    raiseGoal p = p { pgGoal = max (pgGoal p) (withCleanupMargin (pgTotal p)) }

-- | Adds a margin of 5% (rounded up) to a number of cache files, for
-- the estimated goal of the cache cleanup (see 'cleanupCache').
withCleanupMargin :: Int -> Int
withCleanupMargin n = n + (n + 19) `div` 20

-- | Cleans up a given cache file.
cleanCacheFile :: FilePath       -- ^ Absolute prefix path on the filesystem
               -> LogFn          -- ^ Logger function
               -> FilePath       -- ^ Relative path to the item being examined
               -> IO (Progress -> Progress)
cleanCacheFile prefix logger path = do
  res <- deleteCacheFile prefix path
  case res of
    Just err -> do
      logger LevelError $ "Failed to delete path: " <> toLogStr err
      return $ incErrors (Text.pack path) err
    Nothing  -> do
      logger LevelDebug $ "Cleaned obsolete path '" <> toLogStr path <> "'"
      return incDone

-- | Cleanups given directories under the cache dir.
--
-- The cache is walked once. The goal of the progress is therefore not
-- known in advance: it starts from an estimate, the number of files in
-- the repository times the cache files expected for each (see
-- 'expectedCacheFiles'), plus a margin of 5% (see 'withCleanupMargin').
-- It is kept that much above the number of files examined so far (see
-- 'examineCacheFile'), so the progress stops short of complete until
-- the end. Once done, it is set to the number of files actually
-- examined, so that the results add up (and the progress is complete).
cleanupCache :: Ctx         -- ^ Context
             -> Repository  -- ^ Repository with current set of images
             -> [FilePath]  -- ^ Set of directories to cleanup
             -> IO Progress -- ^ Result of cleanup
cleanupCache ctx repo alldirs = do
  let cleanProgress = ctxCleanProgress ctx
      imgs = allRepoFiles repo
      iset = Set.fromList $ map (Text.pack . fileFullPath) imgs
      config = ctxConfig ctx
      cacheDir = cfgCacheDir config
      handler = examineCacheFile cacheDir cleanProgress
                  (cacheFileEndings config) iset (ctxLogger ctx)
      estimate = withCleanupMargin (Set.size iset * expectedCacheFiles config)
  ctxLogger ctx LevelInfo $ "Cleaning up cache items under " <>
    toLogStr (show (map (cacheDir </>) alldirs))
  atomically $ writeTVar cleanProgress $! def { pgGoal = estimate }
  mapM_ (allPathFiles cacheDir handler) alldirs
  atomically $ do
    modifyTVar' cleanProgress $ \p -> p { pgGoal = pgTotal p }
    readTVar cleanProgress

waitForScan :: Ctx -> IO Repository
waitForScan ctx = atomically $ do
  scanner <- readTVar (ctxScanner ctx)
  maybe (readTVar (ctxRepo ctx)) waitSTM scanner

launchScanFileSystem :: Ctx -> IO ()
launchScanFileSystem ctx = do
  -- The new serial is taken and the new scanner registered in a single
  -- transaction, so that the registered scanner is always the one
  -- owning the repository: with two separate steps, two launches at
  -- the same time could each take a serial, and then register their
  -- scanners in the other order, the later serial's scanner being
  -- cancelled by the other launch, and the earlier one failing as it
  -- no longer owns the repository; the status was then left at
  -- "starting", with no scan running.
  --
  -- The scanner is therefore started first, waiting for its serial.
  -- Asynchronous exceptions are masked until it is registered (neither
  -- starting a thread nor a transaction that doesn't wait can be
  -- interrupted), so that it can't be lost; the scanner itself runs
  -- unmasked. If registering it fails, nothing was changed, and the
  -- scanner, which hasn't started, is stopped.
  previous <- mask_ $ do
    serialVar <- newEmptyTMVarIO
    newscanner <- asyncWithUnmask $ \unmask ->
      unmask $ atomically (takeTMVar serialVar) >>= scanFSWrapper ctx
    atomically (do serial <- newRepo rc
                   putTMVar serialVar serial
                   swapTVar scannerThread (Just newscanner))
      `onException` uninterruptibleCancel newscanner
  forM_ previous $ \t -> do
    logfn LevelInfo "Cancelling previous scanner"
    cancel t
    logfn LevelInfo "Cancel done"
  where rc = ctxRepo ctx
        logfn = ctxLogger ctx
        scannerThread = ctxScanner ctx

-- | Runs a scan (see 'runScanAction'), then forces a major garbage
-- collection, so that most of the memory the scan used is released
-- when it ends. Otherwise that waits for the next major collection:
-- an idle one, much later with a long idle interval (e.g.
-- @+RTS -I63@), or one triggered by allocation, which after a scan's
-- large heap can take long even on a busy server. The runtime gives
-- the rest back over the following major collections (see its @-Fd@
-- option), and what it gives back is really released (see the
-- @--disable-delayed-os-memory-return@ runtime option).
--
-- The collection runs when the scan finishes or fails, but not when a
-- newer scan replaces it: being cancelled (an asynchronous exception),
-- or failing to record its failure because a newer scan owns the
-- repository, skips it, as the newer scan collects at its own end, and
-- collecting would only delay it.
scanFSWrapper :: Ctx -> Int -> IO Repository
scanFSWrapper ctx serial = do
  repo <- runScanAction ctx serial (scanFilesystem ctx serial)
  performMajorGC
  return repo

-- | Runs a scan, recording its failure as the repository's status,
-- and returning that repository instead of failing.
--
-- The scan is known by its serial only (see 'newRepo'): on failure the
-- status of the current repository is changed, whatever it holds by
-- then. That is the previous data if the scan failed before publishing
-- its own, and the new data after that; in particular, a failure while
-- rendering or cleaning up doesn't bring back the previous data, with
-- the pictures the scan found to be gone.
--
-- The failure is logged first, so that it isn't lost if it can't be
-- recorded: if a newer scan owns the repository, the status is left
-- alone, and this fails instead.
--
-- Neither being cancelled nor being superseded by a newer scan (see
-- 'OwnershipChanged') is a failure: a newer scan replaces this one, and
-- this one is cancelled soon after, or stops at its next check. The
-- former, an asynchronous exception, is not handled here; the latter
-- is only logged as information, and passed on.
runScanAction :: Ctx -> Int -> IO Repository -> IO Repository
runScanAction ctx !serial action =
  action `catch` \(err :: SomeException) ->
    case fromException err of
      Just OwnershipChanged -> do
        logfn LevelInfo $ "Scan " <> toLogStr (show serial) <>
          " stopped, superseded by a newer scan"
        throwIO err
      Nothing -> do
        let msg = sformat shown err
        logfn LevelError $ "Scan " <> toLogStr (show serial) <>
          " failed: " <> toLogStr msg
        tryUpdateRepoStatus ctx serial (RepoError msg)
  where logfn = ctxLogger ctx

-- | Scans the file system, as the scan with the given serial (see
-- 'newRepo', which also set the starting status).
scanFilesystem :: Ctx -> Int -> IO Repository
scanFilesystem ctx serial = do
  let logfn = ctxLogger ctx
      config = ctxConfig ctx
      scanProgress = ctxScanProgress ctx
  logfn LevelInfo "Launching scan filesystem"
  traceMarkerIO "scanFilesystem start"
  -- Only in profiling builds:
  -- _ <- incrementUserEra 1
  -- A scan already superseded (by another one started at the same
  -- time) stops here, before it resets the scan's progress.
  checkScanOwnership ctx serial
  let srcdirs = map (, True)  (cfgSourceDirs config)
      outdirs = map (, False) (cfgOutputDirs config)
      alldirs = map fst $ srcdirs ++ outdirs
      alldirsAsRelative = map makeRel alldirs
  logfn LevelInfo $ "Counting images under " <> toLogStr (show alldirs)
  itemcounts <- mapConcurrently (countDir config) alldirs
  let cachePaths = map (cfgCacheDir config </>) alldirsAsRelative
  -- Ensure cache top dirs are created if missing (likely only during
  -- bootstrap, but better to always redo during scanning).
  mapConcurrently_ (createDirectoryIfMissing True) cachePaths
  atomically $ writeTVar scanProgress $! def { pgGoal = sum itemcounts }
  start <- getZonedTime
  let ws = WorkStart { wsStart = start }
  _ <- tryUpdateRepoStatus ctx serial RepoScanning { rsScanGoal = ws }
  asyncDirs <- mapConcurrently (uncurry (scanBaseDir ctx))
                 $ srcdirs ++ outdirs
  logfn LevelInfo "Finished scanning directories"
  traceMarkerIO "scanFilesystem scanned dirs"
  scanned <- readTVarIO scanProgress
  end <- getZonedTime
  let repo = foldl' (flip (addDirToRepo config)) Map.empty $ concat asyncDirs
  let repo' = Map.map (resolveProcessedRanges config .
                       mergeShadows config) repo
      stats = computeRepoStats repo'
      rexif = repoGlobalExif repo'
      wrscan = WorkResults { wrStart = start
                           , wrEnd = end
                           , wrDone = scanned
                           }
      wsrender= WorkStart { wsStart = end
                          }
  -- The new repository is built here, in full: it is lazy up to now
  -- (the folders are evaluated, but not their merging, nor the
  -- statistics), and neither the transaction publishing it, nor
  -- whichever request reads it first, should do that work.
  built <- evaluate $ force Repository
             { repoDirs   = repo'
             , repoStats  = stats
             , repoExif   = rexif
             , repoStatus = RepoRendering { rsScanResults = wrscan,
                                            rsRenderGoal = wsrender }
             , repoSerial = serial
             }
  -- From here on, and until this scan writes its own once finished, a
  -- restart must not load the previous scan's cache: its pictures
  -- might be gone since. Only if this scan is still the current one,
  -- though: the cache is not a superseded scan's to remove.
  --
  -- The check and the removal are not atomic (the removal can't be
  -- part of a transaction): a scan superseded right between the two
  -- still removes the cache. That is the safe side to err on: the new
  -- scan removes it as well once it has its data, so the only effect
  -- is that a restart before that scans again, instead of loading the
  -- previous scan's cache; a restart never loads data older than it
  -- would have otherwise.
  checkScanOwnership ctx serial
  removeDiskCache config
  -- The rendering's progress is reset before the new status shows, as
  -- the status page would otherwise show the previous rendering's as
  -- this one's, until forceBuildThumbCaches resets it, with the number
  -- of images to check. That number isn't needed here: any goal makes
  -- the page show a check that has started, so it is given one image.
  -- The progress is only shown while rendering, so resetting it early
  -- is not seen.
  atomically $ resetRenderProgress ctx 1
  repo_as <- tryUpdateRepo ctx built
  logfn LevelInfo "Finished building repo, starting rendering"
  traceMarkerIO "scanFilesystem start rendering"
  rendered <- forceBuildThumbCaches ctx repo_as
  endr <- getZonedTime
  let wrrender = WorkResults { wrStart = end
                             , wrEnd = endr
                             , wrDone = rendered
                             }
      wrstatus = RepoCleaning { rsScanResults = wrscan
                              , rsRenderResults = wrrender
                              , rsCleanGoal = WorkStart { wsStart = endr }
                              }
  repo_ar <- tryUpdateRepoStatus ctx serial wrstatus
  logfn LevelInfo "Finished rendering, starting cleanup"
  traceMarkerIO "scanFilesystem start cleanup"
  clean_pg <- cleanupCache ctx repo_ar alldirsAsRelative
  endc <- getZonedTime
  let wrclean = WorkResults { wrStart = endr
                            , wrEnd = endc
                            , wrDone = clean_pg
                            }
      status = RepoFinished { rsScanResults = wrscan
                            , rsRenderResults = wrrender
                            , rsCleanResults = wrclean
                            }
  r4 <- tryUpdateRepoStatus ctx serial status
  writeDiskCache config r4
  logfn LevelInfo "Finished cleaning up, all done"
  traceMarkerIO "scanFilesystem done"
  return r4

-- | Computes the list of images that can be rendered.
renderableImages :: Repository -> [Image]
renderableImages =
  filter (not . null . allViewableImageFiles) .
  filterImagesByClass [ImageUnprocessed, ImageProcessed, ImageStandalone]

-- | Resets the rendering's progress: nothing rendered yet, and the
-- given number of renderable images (see 'renderableImages') left to
-- check (see 'forceBuildThumbCaches').
resetRenderProgress :: Ctx -> Int -> STM ()
resetRenderProgress ctx images = do
  writeTVar (ctxRenderProgress ctx) def
  -- Strict, so that a status request doesn't do the count.
  writeTVar (ctxRenderCheckProgress ctx) $! def { pgGoal = images }

-- | Pre-render all auto image sizes, converting only stale caches.
--
-- First classifies every image×size, checking only the cache files
-- (see 'imagePendingSizes'; no tool runs), then builds the stale ones,
-- including any extraction they need. Render progress 'pgGoal' is the
-- pending work, not the classified total; the classification has its
-- own progress ('ctxRenderCheckProgress'), counting the images
-- checked.
forceBuildThumbCaches :: Ctx -> Repository -> IO Progress
forceBuildThumbCaches ctx repo = do
  checkRepoOwnership ctx repo
  let config = ctxConfig ctx
      renderProgress = ctxRenderProgress ctx
      checkProgress = ctxRenderCheckProgress ctx
      images = renderableImages repo
      sizes = Set.toList (cfgAutoImageSizes config)
      imageForError i = sformat (stext % "/" % stext % " at resolution " % int)
                         (TS.toText $ imgParent i) (TS.toText . unImageName . imgName $ i)
      -- The last image is counted along with setting the rendering's
      -- goal (below), so that the check is never complete before the
      -- goal is set.
      --
      -- The accumulator is strict, and its lists are built by
      -- prepending evaluated elements (their order is not defined):
      -- otherwise it would be a chain of unevaluated updates, as long
      -- as the list of images.
      classifyImage storageDirs total (StrictTriple pend n es) (k, i) = do
        checkRepoOwnership ctx repo
        res <- imagePendingSizes (ctxLogger ctx) config storageDirs i sizes
        when (k < total) . atomically $ modifyTVar' checkProgress incDone
        return $! case res of
          Left err ->
            let msg = Text.pack $ show err
                failed acc size =
                  let !e = ProgressError (imageForError i size) msg
                  in e : acc
            in StrictTriple pend n (foldl' failed es sizes)
          Right stale ->
            StrictTriple (foldl' (\acc size -> (i, size) : acc) pend stale)
                         (n + length sizes - length stale)
                         es
  -- Counted here: not in the transactions below, and not by a status
  -- request either.
  total <- evaluate $ length images
  atomically $ resetRenderProgress ctx total
  -- For this check only: the builds below, as the requests, check the
  -- files with the directories resolved each time.
  storageDirs <- resolveStorageDirs (ctxLogger ctx) config
  StrictTriple pending noops errs <-
    foldM (classifyImage storageDirs total) (StrictTriple [] 0 []) (zip [1..] images)
  goal <- evaluate $ length pending + length errs
  -- The check is marked complete explicitly, not relying on its count.
  atomically $ do
    writeTVar renderProgress $! Progress
      { pgErrors = errs
      , pgNoop = noops
      , pgDone = 0
      , pgGoal = goal
      }
    modifyTVar' checkProgress $ \p -> p { pgDone = pgGoal p }
  mapM_ (\(i, size) -> do
            checkRepoOwnership ctx repo
            res <- imageAtRes (ctxRenderToolLimits ctx) (ctxLogger ctx) config i . Just . ImageSize $ size
            let modifier = case res of
                  Left   err      -> incErrors (imageForError i size) (Text.pack $ show err)
                  Right (_, _, _) -> incDone
            atomically $ modifyTVar' renderProgress modifier
        ) pending
  readTVarIO renderProgress

repoDiskFile :: String
repoDiskFile = "repo"

repoDiskPath :: Config -> FilePath -> FilePath
repoDiskPath config path = cachedBasename config path "cache"

-- | The path of the repository cache.
repoCachePath :: Config -> FilePath
repoCachePath config = repoDiskPath config repoDiskFile

writeDiskCache :: Config -> Repository -> IO ()
writeDiskCache config repo =
  writeCacheFile config repoDiskFile repoDiskPath (encodeRepository (config, repo))

-- | Removes the repository cache, if there is one.
--
-- Any failure other than the cache not existing is an error (and thus
-- fails the scan doing this), as otherwise a restart would load a
-- cache which was meant to be gone. The error says what was being
-- done, as it is what the status page then shows.
removeDiskCache :: Config -> IO ()
removeDiskCache config =
  removeFile path `catch` \e ->
    unless (isDoesNotExistError e) . throwString $
      "Failed to remove the previous repository cache (" ++ path ++
      "), not using the scan's results: " ++ show (e :: IOException)
  where path = repoCachePath config

readDiskCache :: Config -> IO (Maybe Repository)
readDiskCache config = do
  contents <- readCacheFile config repoDiskFile repoDiskPath False []
  -- A cache of another format version (or from before versions) fails
  -- to decode, so it is ignored, as one built with another
  -- configuration is.
  let decoded = contents >>= either (const Nothing) Just . decodeRepository
  return $ case decoded of
    Nothing -> Nothing
    Just (c, repo) -> if c == config
                      then Just repo
                      else Nothing

loadCacheOrScan :: Ctx
                -> Repository
                -> IO Repository
loadCacheOrScan ctx old@(repoStatus -> RepoEmpty) = do
  cachedRepo <- readDiskCache (ctxConfig ctx)
  let rescanning = rescanningRepository old
      logfn = ctxLogger ctx
  case cachedRepo of
     Nothing    -> do
       logfn LevelInfo "No cache data or data incompatible, scanning filesystem"
       launchScanFileSystem ctx
       return rescanning
     Just cached_r@(repoStatus -> RepoFinished {}) -> do
       logfn LevelInfo "Cached data available, skipping scan"
       -- Note: this shouldn't fail, since an empty repo happens only
       -- upon initial load, and (initial) empty repos have a serial
       -- number of zero while any valid cache will have a positive
       -- serial.
       tryUpdateRepo ctx cached_r
     -- Not written by the current code, as a scan removes the cache
     -- rather than writing an unfinished one; handled defensively.
     Just unfinished -> do
       logfn LevelWarn . toLogStr $ "Unfinished cache found, state: " ++ show (repoStatus unfinished)
       logfn LevelInfo "Restarting scan"
       launchScanFileSystem ctx
       return rescanning
loadCacheOrScan _ orig = return orig

scanAll :: Ctx -> IO Repository
scanAll ctx = do
  current <- getRepo ctx
  loadCacheOrScan ctx current
