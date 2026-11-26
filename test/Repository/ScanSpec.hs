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

module Repository.ScanSpec (spec) where

import qualified Data.ByteString      as BS
import           Data.Default
import           Data.Either      (isLeft, isRight)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Store
import qualified Data.Text.Short  as TS
import           Data.Time        (getZonedTime)
import           Control.Concurrent.STM.TSem (signalTSem, waitTSem)
import           Data.Char        (isDigit)
import qualified Data.Text        as Text
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   doesFileExist, doesPathExist)
import           System.FilePath  (pathSeparator, takeDirectory, takeFileName)
import           System.Posix.Files (FileStatus, createNamedPipe,
                                     createSymbolicLink,
                                     getSymbolicLinkStatus,
                                     ownerReadMode, ownerWriteMode,
                                     unionFileModes)
import           Control.Monad.Logger (LogLevel (..))
import           System.Log.FastLogger (fromLogStr)

import           Cache              (cacheTempDir, cachedBasename)
import           Exif
import           Pics
import           Repository
import           Repository.Build
import           Repository.Encoding (encodeRepository)
import           Repository.Scan
import           Repository.State
import           Utils.Yaml         (maxYamlSize)
import           TestImport

isOwnershipAbort :: SomeException -> Bool
isOwnershipAbort e = "ownership changed" `isInfixOf` show e

-- | Launches a scan which blocks when it renders its previews, with its
-- data already published: in a new context (with the configuration and
-- logger of the given one) whose only render slot is taken, and with a
-- picture to render.
--
-- The first action runs before the scan is launched, and the second
-- one, given what the first returned, while the scan is blocked; the
-- slot is freed after it, and the scan goes on (if it fails, the scan
-- is stopped instead). Returns the new context.
withScanBlockedRendering :: Ctx -> (Ctx -> IO a) -> (Ctx -> a -> IO ()) -> IO Ctx
withScanBlockedRendering ctx0 prepare during = do
  let config = ctxConfig ctx0
  renderTools <- atomically $ newToolLimits 1 defaultRenderToolTimeout
  ctx <- atomically $ do
    scanTools <- newToolLimits 1 defaultScanToolTimeout
    initContext config (ctxLogger ctx0) renderTools scanTools
  case cfgOutputDirs config of
    outRoot:_ -> do
      let dir = outRoot </> "2020" </> "2020-01-01-trip"
      createDirectoryIfMissing True dir
      writeRealJpeg (dir </> "a.jpg")
    [] -> expectationFailure "expected an output directory"
  prepared <- prepare ctx
  atomically . waitTSem $ tlSlots renderTools
  (do launchScanFileSystem ctx
      waitUntilBlockedRendering ctx
      during ctx prepared)
    `finally` atomically (signalTSem $ tlSlots renderTools)
    -- On a failure the scan is stopped too (and waited for), so that
    -- it doesn't run on while the test's directories are removed.
    `onException` (readTVarIO (ctxScanner ctx) >>= mapM_ cancel)
  return ctx

-- | Waits until the current scan has published its data and is blocked,
-- which, when the render slots are all taken, is waiting for one.
waitUntilBlockedRendering :: Ctx -> IO ()
waitUntilBlockedRendering ctx = do
  published <- timeout lastResort . atomically $ do
    status <- repoStatus <$> readTVar (ctxRepo ctx)
    case status of
      RepoRendering {} -> return True
      RepoError _      -> return False
      _                -> retrySTM
  published `shouldBe` Just True
  readTVarIO (ctxScanner ctx) >>= \case
    Just scanner -> waitUntilBlockedInSTM (asyncThreadId scanner)
    Nothing      -> expectationFailure "expected a scanner"

-- | Whether a repository is (re)starting a scan.
isStarting :: RepoStatus -> Bool
isStarting RepoStarting = True
isStarting _            = False

-- | Whether a repository has finished its scan.
isFinished :: RepoStatus -> Bool
isFinished RepoFinished {} = True
isFinished _               = False

spec :: Spec
spec = parallel $ do
  describe "folderYamlFile" $ do
    let sized size = (mkInode "corydalis.yaml") { inodeSize = size }
    it "picks the folder's own file" $
      folderYamlFile [mkInode "a.nef", mkInode "corydalis.yaml"]
        `shouldBe` Just (Right "corydalis.yaml")
    it "ignores files of the same name in subdirectories" $
      folderYamlFile [(mkInode "corydalis.yaml") { inodeDirs = ["sub"] }]
        `shouldBe` Nothing
    it "returns nothing without such a file" $
      folderYamlFile [mkInode "a.nef"] `shouldBe` Nothing
    it "accepts a file of exactly the limit" $
      folderYamlFile [sized (fromIntegral maxYamlSize)]
        `shouldBe` Just (Right "corydalis.yaml")
    it "refuses a file larger than the limit" $
      folderYamlFile [sized (fromIntegral maxYamlSize + 1)]
        `shouldSatisfy` maybe False isLeft

  describe "makeRel" $ do
    it "strips leading separators" $
      makeRel "/foo/bar" `shouldBe` "foo/bar"
    it "leaves relative paths unchanged" $
      makeRel "foo/bar" `shouldBe` "foo/bar"

  withConfig $ do
    describe "isOKDir" $ do
      it "matches date-prefixed folder names" $ \config -> do
        isOKDir config "2024-01-01-trip" `shouldBe` True
        isOKDir config "not-a-date" `shouldBe` False
        isOKDir config "level1" `shouldBe` False

    describe "isLiveCacheFile" $ do
      let files = Set.fromList ["/p/a.nef", "/p/a.nef copy.jpg"]
          live config = isLiveCacheFile (cacheFileEndings config) files
      it "accepts a file's path, a dash, and a suffix in use" $ \config -> do
        forM_ (Set.toList (cfgAllImageSizes config)) $ \size ->
          live config ("/p/a.nef-" <> tshow size) `shouldBe` True
        forM_ ["embedded", "exif", "bexif"] $ \suffix ->
          live config ("/p/a.nef-" <> suffix) `shouldBe` True
      it "accepts the files of a picture another's name starts with" $ \config -> do
        -- "a.nef copy.jpg" sorts between "a.nef" and "a.nef-exif", which
        -- defeated the earlier check (the largest repository path not
        -- after the cache file's had to be its prefix).
        live config "/p/a.nef-exif" `shouldBe` True
        live config "/p/a.nef copy.jpg-exif" `shouldBe` True
      it "refuses the files of files not in the repository" $ \config -> do
        live config "/p/b.nef-exif" `shouldBe` False
        -- Paths starting with a repository file's path, which the
        -- earlier check kept.
        live config "/p/a.nef copy 2.jpg-exif" `shouldBe` False
        isLiveCacheFile (cacheFileEndings config) (Set.singleton "/p/a")
          "/p/a.nef-exif" `shouldBe` False
      it "accepts the paths of all kinds of cache files" $ \config -> do
        let picture = "/p/a.nef"
            -- The paths without the cache directory, as the cleanup
            -- sees them.
            relative path = fromMaybe path $
              stripPrefix (cfgCacheDir config ++ "/") path
            paths = embeddedImagePath config picture :
                    exifPath config picture :
                    bExifPath config picture :
                    map (scaledImagePath config picture)
                        (Set.toList (cfgAllImageSizes config))
        forM_ paths $ \path ->
          (path, live config (pack (relative path))) `shouldBe` (path, True)
      it "refuses suffixes not in use" $ \config -> do
        let unused = Set.findMax (cfgAllImageSizes config) + 1
        live config ("/p/a.nef-" <> tshow unused) `shouldBe` False
        live config "/p/a.nef-other" `shouldBe` False
        live config "/p/a.nef-exif-dev" `shouldBe` False
        live config "/p/a.nef" `shouldBe` False
      it "refuses the temporary files left by older versions" $ \config ->
        live config "/p/a.nef-exif123456.tmp" `shouldBe` False

  withUnscannedContext $ do
    describe "loading the repository cache" $ do
      let cached status = startupRepository { repoStatus = status, repoSerial = 5 }
          finished now = RepoFinished (done now) (done now) (done now)
          done now = WorkResults now now def
      it "scans the file system without a cache" $ \ctx -> do
        repo <- scanAll ctx
        repoStatus repo `shouldSatisfy` isStarting
        void $ waitForScan ctx
      it "uses a finished cached repository, without scanning" $ \ctx -> do
        now <- getZonedTime
        writeDiskCache (ctxConfig ctx) (cached (finished now))
        repo <- scanAll ctx
        repoSerial repo `shouldBe` 5
        repoStatus repo `shouldSatisfy` isFinished
        current <- getRepo ctx
        repoSerial current `shouldBe` 5
      it "scans again after an unfinished cached repository" $ \ctx -> do
        now <- getZonedTime
        writeDiskCache (ctxConfig ctx) (cached (RepoScanning (WorkStart now)))
        repo <- scanAll ctx
        repoStatus repo `shouldSatisfy` isStarting
        final <- waitForScan ctx
        repoStatus final `shouldSatisfy` isFinished
      it "ignores a cache from another configuration" $ \ctx -> do
        now <- getZonedTime
        let other = (ctxConfig ctx) { cfgThumbnailSize = cfgThumbnailSize (ctxConfig ctx) + 1 }
        writeDiskCache other (cached (finished now))
        repo <- scanAll ctx
        repoStatus repo `shouldSatisfy` isStarting
        void $ waitForScan ctx
      it "ignores a cache without a format version" $ \ctx -> do
        now <- getZonedTime
        let config = ctxConfig ctx
        -- As written before the cache had a format version.
        BS.writeFile (repoCachePath config)
          (Data.Store.encode (config, cached (finished now)))
        repo <- scanAll ctx
        repoStatus repo `shouldSatisfy` isStarting
        void $ waitForScan ctx
      it "ignores a cache of another format version" $ \ctx -> do
        now <- getZonedTime
        let config = ctxConfig ctx
            encoded = encodeRepository (config, cached (finished now))
        BS.writeFile (repoCachePath config) (otherCacheVersion encoded)
        repo <- scanAll ctx
        repoStatus repo `shouldSatisfy` isStarting
        void $ waitForScan ctx
      it "doesn't load the cache again once loaded" $ \ctx -> do
        now <- getZonedTime
        writeDiskCache (ctxConfig ctx) (cached (finished now))
        _ <- scanAll ctx
        writeDiskCache (ctxConfig ctx) (cached (RepoScanning (WorkStart now)))
        repo <- scanAll ctx
        repoStatus repo `shouldSatisfy` isFinished
      it "removes the previous cache once a scan has new data" $ \ctx0 -> do
        let config = ctxConfig ctx0
            cachePath = repoCachePath config
            prepare _ = do
              now <- getZonedTime
              writeDiskCache config (cached (finished now))
              doesFileExist cachePath `shouldReturn` True
        ctx <- withScanBlockedRendering ctx0 prepare $ \_ () -> do
          -- A restart now scans again, rather than loading the
          -- previous scan's results.
          doesFileExist cachePath `shouldReturn` False
        final <- waitForScan ctx
        repoStatus final `shouldSatisfy` isFinished
        doesFileExist cachePath `shouldReturn` True
        -- A restart now loads the scan's results.
        restarted <- atomically $ do
          renderTools <- newToolLimits 1 defaultRenderToolTimeout
          scanTools <- newToolLimits 1 defaultScanToolTimeout
          initContext config (ctxLogger ctx0) renderTools scanTools
        loaded <- scanAll restarted
        repoStatus loaded `shouldSatisfy` isFinished
        repoSerial loaded `shouldBe` repoSerial final
        Map.keys (repoDirs loaded) `shouldBe` ["2020-01-01-trip"]
      it "keeps the previous cache when the scan was superseded" $ \ctx0 -> do
        requireExifTool
        let config = ctxConfig ctx0
            cachePath = repoCachePath config
        scanTools <- atomically $ newToolLimits 1 defaultScanToolTimeout
        ctx <- atomically $ do
          renderTools <- newToolLimits 1 defaultRenderToolTimeout
          initContext config (ctxLogger ctx0) renderTools scanTools
        case cfgOutputDirs config of
          outRoot:_ -> do
            let dir = outRoot </> "2020" </> "2020-01-01-trip"
            createDirectoryIfMissing True dir
            writeRealJpeg (dir </> "a.jpg")
          [] -> expectationFailure "expected an output directory"
        now <- getZonedTime
        writeDiskCache config (cached (finished now))
        -- Taking the only slot, so that the scan waits for it when it
        -- reads the picture's metadata.
        atomically . waitTSem $ tlSlots scanTools
        (do launchScanFileSystem ctx
            scanning <- timeout lastResort . atomically $ do
              status <- repoStatus <$> readTVar (ctxRepo ctx)
              case status of
                RepoScanning {} -> return True
                RepoStarting    -> retrySTM
                _               -> return False
            scanning `shouldBe` Just True
            -- As another scan would, without running one.
            void . atomically $ newRepo (ctxRepo ctx))
          `finally` atomically (signalTSem $ tlSlots scanTools)
        waitForScan ctx `shouldThrow` isOwnershipAbort
        -- The cache wasn't this scan's to remove any more.
        doesFileExist cachePath `shouldReturn` True
      it "fails the scan if the previous cache can't be removed" $ \ctx -> do
        let cachePath = repoCachePath (ctxConfig ctx)
        -- A directory can't be removed as a file.
        createDirectoryIfMissing True cachePath
        launchScanFileSystem ctx
        final <- waitForScan ctx
        case repoStatus final of
          -- The error names what couldn't be removed.
          RepoError msg -> unpack msg `shouldSatisfy` (cachePath `isInfixOf`)
          other         -> expectationFailure $ "expected RepoError, got " ++ show other
        doesDirectoryExist cachePath `shouldReturn` True

    describe "scan results" $ do
      it "replaces a running scan with a new one" $ \ctx0 -> do
        ctx <- withScanBlockedRendering ctx0 (const $ return ()) $ \ctx () -> do
          firstScanner <- readTVarIO (ctxScanner ctx)
          firstSerial <- repoSerial <$> getRepo ctx
          -- This cancels the first scan, and waits for it to stop.
          launchScanFileSystem ctx
          case firstScanner of
            Just scanner -> do
              result <- waitCatch scanner
              (either fromException (const Nothing) result :: Maybe AsyncCancelled)
                `shouldBe` Just AsyncCancelled
            Nothing -> expectationFailure "expected a scanner"
          -- The new scan blocks the same way, with its own data.
          waitUntilBlockedRendering ctx
          (repoSerial <$> getRepo ctx) `shouldReturn` firstSerial + 1
        final <- waitForScan ctx
        repoStatus final `shouldSatisfy` isFinished
        Map.keys (repoDirs final) `shouldBe` ["2020-01-01-trip"]
      it "keeps a scan running when scans are launched at the same time" $ \ctx ->
        -- Each launch takes a new serial and registers its scanner; the
        -- scanner left registered must be the one owning the repository.
        -- The launches race, so a regression is only caught some of the
        -- time: with the race reintroduced, one round failed 8 of 20
        -- runs, and five rounds 20 of 20.
        forM_ [1 :: Int .. 5] $ \_ ->
          (do replicateConcurrently_ 20 (launchScanFileSystem ctx)
              final <- timeout lastResort (waitForScan ctx)
              case final of
                Just repo -> do
                  repoStatus repo `shouldSatisfy` isFinished
                  (repoSerial <$> getRepo ctx) `shouldReturn` repoSerial repo
                Nothing -> expectationFailure "the scan didn't finish")
            -- On a failure the scan is stopped too (and waited for), so
            -- that it doesn't run on while the test's directories are
            -- removed.
            `onException` (readTVarIO (ctxScanner ctx) >>= mapM_ cancel)

    describe "scan tool limits" $ do
      -- Scans a folder with a picture, with the given time limits for
      -- the preview tools and the scan's exiftool runs, returning the
      -- exif warnings of the picture.
      let scanWith ctx0 renderTimeout scanTimeout = do
            let config = ctxConfig ctx0
            ctx <- atomically $ do
              renderTools <- newToolLimits 1 renderTimeout
              scanTools <- newToolLimits 1 scanTimeout
              initContext config (ctxLogger ctx0) renderTools scanTools
            case cfgOutputDirs config of
              outRoot:_ -> do
                -- Folders are two levels below the base directory, and
                -- their names must match the folder pattern.
                let dir = outRoot </> "2020" </> "2020-01-01-trip"
                createDirectoryIfMissing True dir
                writeRealJpeg (dir </> "a.jpg")
                launchScanFileSystem ctx
                repo <- waitForScan ctx
                return [ map TS.toText . Set.toList . exifWarning $ fileExif f
                       | f <- allRepoFiles repo, fileName f == "a.jpg" ]
              [] -> expectationFailure "expected an output directory" >> return []
          timedOut = any ("timed out" `isInfixOf`)
      -- A microsecond is too short for any exiftool run.
      it "runs exiftool within the scan's limits" $ \ctx0 -> do
        requireExifTool
        warnings <- scanWith ctx0 defaultRenderToolTimeout 1
        warnings `shouldSatisfy` (\ws -> not (null ws) && all timedOut ws)
      it "doesn't run exiftool within the previews' limits" $ \ctx0 -> do
        requireExifTool
        warnings <- scanWith ctx0 1 defaultRenderToolTimeout
        warnings `shouldSatisfy` (\ws -> not (null ws) && not (any timedOut ws))

    describe "cache cleanup" $ do
      it "keeps only the cache files in use, and the files being written" $ \ctx -> do
        let config = ctxConfig ctx
        case (cfgOutputDirs config, Set.toList (cfgAllImageSizes config)) of
          (outRoot:_, size:_) -> do
            -- Folders are two levels below the base directory, and
            -- their names must match the folder pattern.
            let srcDir = outRoot </> "2020" </> "2020-01-01-trip"
                picture = srcDir </> "a.jpg"
                -- Its name starts with the first picture's, and sorts
                -- before the first picture's cache files.
                other = srcDir </> "a.jpg copy.jpg"
                unusedSize = Set.findMax (cfgAllImageSizes config) + 1
                live = [ scaledImagePath config picture size
                       , scaledImagePath config other size
                       , embeddedImagePath config picture
                       ]
                obsolete = [ scaledImagePath config (srcDir </> "gone.jpg") size
                           , scaledImagePath config picture unusedSize
                           , scaledImagePath config other unusedSize
                           , cachedBasename config picture "other"
                           -- A removed copy, whose name starts with
                           -- the first picture's.
                           , scaledImagePath config (srcDir </> "a.jpg copy 2.jpg") size
                           , scaledImagePath config picture size ++ "123456.tmp"
                           ]
                beingWritten = cacheTempDir config </> ("a.jpg-" ++ show size ++ "123.tmp")
            createDirectoryIfMissing True srcDir
            writeRealJpeg picture
            writeRealJpeg other
            mapM_ writeNonEmpty (live ++ obsolete ++ [beingWritten])
            launchScanFileSystem ctx
            repo <- waitForScan ctx
            case repoStatus repo of
              RepoFinished { rsCleanResults = WorkResults { wrDone = cleaned } } ->
                -- Its goal is what it processed, so that the results add
                -- up even if the cache changes while it is cleaned.
                pgGoal cleaned `shouldBe` pgTotal cleaned
              status -> expectationFailure $ "The scan failed: " ++ show status
            -- With the path, to name the file in a failure.
            let present path = (,) path <$> doesFileExist path
            forM_ live $ \path -> present path `shouldReturn` (path, True)
            forM_ obsolete $ \path -> present path `shouldReturn` (path, False)
            doesFileExist beingWritten `shouldReturn` True
            -- Written by the scan.
            doesFileExist (exifPath config picture) `shouldReturn` True
            doesFileExist (bExifPath config picture) `shouldReturn` True
          _ -> expectationFailure "expected an output directory and image sizes"
      it "removes links and special files, without following the links" $ \ctx -> do
        let config = ctxConfig ctx
        case (cfgOutputDirs config, Set.toList (cfgAllImageSizes config)) of
          (outRoot:_, size:_) -> do
            let srcDir = outRoot </> "2020" </> "2020-01-01-trip"
                picture = srcDir </> "a.jpg"
                live = scaledImagePath config picture size
                cacheFolder = takeDirectory live
                -- Outside of the cache, with a file named as the
                -- picture's cache file, which a walk following the
                -- link would find.
                outside = takeDirectory outRoot </> "outside"
                outsideFile = outside </> takeFileName live
                dirLink = cacheFolder </> "linked-dir"
                fileLink = cacheFolder </> "linked-file"
                pipe = cacheFolder </> "pipe"
            createDirectoryIfMissing True srcDir
            writeRealJpeg picture
            writeNonEmpty live
            writeNonEmpty outsideFile
            createSymbolicLink outside dirLink
            createSymbolicLink outsideFile fileLink
            createNamedPipe pipe 0o600
            launchScanFileSystem ctx
            repo <- waitForScan ctx
            case repoStatus repo of
              RepoFinished {} -> return ()
              status -> expectationFailure $ "The scan failed: " ++ show status
            let present path = (,) path <$> doesPathExist path
                -- A dangling link counts too.
                entryExists path = (,) path . isRight <$>
                  (try (getSymbolicLinkStatus path) :: IO (Either IOException FileStatus))
            forM_ [dirLink, fileLink, pipe] $ \path ->
              entryExists path `shouldReturn` (path, False)
            present live `shouldReturn` (live, True)
            present outsideFile `shouldReturn` (outsideFile, True)
          _ -> expectationFailure "expected an output directory and image sizes"
      it "keeps its estimated goal 5% above the files examined" $ \ctx -> do
        let config = ctxConfig ctx
            path = "dir" </> "a.jpg-64"
            iset = Set.singleton (Text.pack $ pathSeparator : "dir" </> "a.jpg")
            examine n pg = do
              progress <- newTVarIO pg
              -- A file in use, so that nothing is deleted.
              replicateM_ n $
                examineCacheFile (cfgCacheDir config) progress ["-64"] iset
                  (\_ _ -> return ()) path
              readTVarIO progress
        raised <- examine 40 def { pgGoal = 1 }
        (pgGoal raised, pgNoop raised) `shouldBe` (42, 40)
        -- Rounded up, so never complete before the end.
        few <- examine 3 def { pgGoal = 1 }
        (pgGoal few, pgNoop few) `shouldBe` (4, 3)
        kept <- examine 40 def { pgGoal = 100 }
        (pgGoal kept, pgNoop kept) `shouldBe` (100, 40)

    describe "scan abort" $ do
      let errorMessage repo = case repoStatus repo of
            RepoError msg -> return msg
            other         -> do
              expectationFailure $ "expected RepoError, got " ++ show other
              return ""
          folders = Map.keys . repoDirs
          setFolders ctx names = atomically . modifyTVar' (ctxRepo ctx) $ \r ->
            r { repoDirs = Map.fromList [(n, createTestPicDir n) | n <- names] }
      it "records synchronous scan failures as RepoError" $ \ctx -> do
        serial <- atomically $ newRepo (ctxRepo ctx)
        result <- runScanAction ctx serial (throwString "scan exploded")
        errorMessage result >>= (`shouldSatisfy` ("scan exploded" `isInfixOf`))
        repo' <- getRepo ctx
        errorMessage repo' >>= (`shouldSatisfy` ("scan exploded" `isInfixOf`))
        repoSerial repo' `shouldBe` serial
      -- A context logging into a list, the latest message first.
      let withLogs ctx0 = do
            logs <- newIORef []
            let logfn level msg = atomicModifyIORef' logs (\l -> ((level, fromLogStr msg):l, ()))
            ctx <- atomically $
              initContext (ctxConfig ctx0) logfn (ctxRenderToolLimits ctx0)
                (ctxScanToolLimits ctx0)
            return (ctx, readIORef logs)
          loggedErrors = map snd . filter ((== LevelError) . fst)
      it "logs scan failures" $ \ctx0 -> do
        (ctx, logged) <- withLogs ctx0
        serial <- atomically $ newRepo (ctxRepo ctx)
        _ <- runScanAction ctx serial (throwString "scan exploded")
        errors <- loggedErrors <$> logged
        errors `shouldSatisfy` any (\m -> "scan exploded" `BS.isInfixOf` m &&
                                         ("Scan " <> fromString (show serial)) `BS.isInfixOf` m)
      it "logs the failure of a superseded scan, which it can't record" $ \ctx0 -> do
        (ctx, logged) <- withLogs ctx0
        serial <- atomically $ newRepo (ctxRepo ctx)
        _ <- atomically $ newRepo (ctxRepo ctx)
        runScanAction ctx serial (throwString "scan exploded")
          `shouldThrow` isOwnershipAbort
        errors <- loggedErrors <$> logged
        errors `shouldSatisfy` any ("scan exploded" `BS.isInfixOf`)
      it "doesn't log a superseded scan as failed" $ \ctx0 -> do
        (ctx, logged) <- withLogs ctx0
        serial <- atomically $ newRepo (ctxRepo ctx)
        repo <- getRepo ctx
        _ <- atomically $ newRepo (ctxRepo ctx)
        -- Stops at its first check of the repository's ownership.
        runScanAction ctx serial (forceBuildThumbCaches ctx repo >> getRepo ctx)
          `shouldThrow` isOwnershipAbort
        loggedErrors <$> logged `shouldReturn` []
      it "keeps the previous data when a scan fails before publishing" $ \ctx -> do
        setFolders ctx ["previous"]
        serial <- atomically $ newRepo (ctxRepo ctx)
        result <- runScanAction ctx serial (throwString "scan exploded")
        folders result `shouldBe` ["previous"]
        repo' <- getRepo ctx
        folders repo' `shouldBe` ["previous"]
        void $ errorMessage repo'
      it "keeps the new data when a scan fails after publishing" $ \ctx -> do
        setFolders ctx ["previous"]
        serial <- atomically $ newRepo (ctxRepo ctx)
        result <- runScanAction ctx serial $ do
          -- As the scan does: new data, under the scan's serial.
          setFolders ctx ["new"]
          throwString "render exploded"
        folders result `shouldBe` ["new"]
        repo' <- getRepo ctx
        folders repo' `shouldBe` ["new"]
        errorMessage repo' >>= (`shouldSatisfy` ("render exploded" `isInfixOf`))
        repoSerial repo' `shouldBe` serial
      it "leaves the repository alone when a superseded scan fails" $ \ctx -> do
        serial <- atomically $ newRepo (ctxRepo ctx)
        newer <- atomically $ newRepo (ctxRepo ctx)
        newer `shouldBe` serial + 1
        runScanAction ctx serial (throwString "scan exploded")
          `shouldThrow` isOwnershipAbort
        repo' <- getRepo ctx
        repoSerial repo' `shouldBe` newer
        repoStatus repo' `shouldSatisfy` isStarting
      it "stops scan work when the scanner thread is cancelled" $ \ctx -> do
        started <- newEmptyMVar
        block <- newEmptyMVar
        cleaned <- newIORef False
        repo <- getRepo ctx
        scanner <- async $ runScanAction ctx (repoSerial repo) $
          (putMVar started () >> takeMVar block >> return repo)
          `finally` writeIORef cleaned True
        takeMVar started
        cancel scanner
        readIORef cleaned `shouldReturn` True
        -- Being cancelled is not a failure of the scan.
        repo' <- getRepo ctx
        case repoStatus repo' of
          RepoError msg -> expectationFailure $ "unexpected error: " ++ show msg
          _             -> return ()
      it "aborts thumbnail builds after a newer scan takes ownership" $ \ctx -> do
        old <- getRepo ctx
        _ <- atomically $ newRepo (ctxRepo ctx)
        forceBuildThumbCaches ctx old `shouldThrow` isOwnershipAbort

    describe "forceBuildThumbCaches" $ do
      it "leaves the render goal at zero when nothing is renderable" $ \ctx -> do
        repo <- getRepo ctx
        pg <- forceBuildThumbCaches ctx repo
        pgGoal pg `shouldBe` 0
        pgNoop pg `shouldBe` 0
        pgDone pg `shouldBe` 0
        pgNumErrors pg `shouldBe` 0
      it "does not render hidden components" $ \ctx -> do
        let dir = compositeFolder (ctxConfig ctx)
                    [[("d_01.nef", 10), ("d_02.nef", 10), ("d_03.nef", 10)]]
                    [[("d_01-03.jpg", 50), ("d_02.jpg", 5)]]
            repo = mkRepository (Map.singleton "f" dir)
        -- d_02 has its own JPEG, so it is not hidden.
        map imgName (renderableImages repo) `shouldMatchList` ["d_01-03", "d_02"]
      it "does not try to render untracked-only files" $ \ctx -> do
        repo <- getRepo ctx
        let config = ctxConfig ctx
            untracked = mkImage config "sums" "folder" Nothing Nothing [] Nothing []
                          [simpleFile "SHA1SUMS"] Nothing MediaUnknown def
            dirs = addImageToRepo (repoDirs repo) untracked
            repo' = repo { repoDirs = dirs }
        null (renderableImages repo') `shouldBe` True
        pg <- forceBuildThumbCaches ctx repo'
        pgGoal pg `shouldBe` 0
        pgNoop pg `shouldBe` 0
        pgDone pg `shouldBe` 0
        pgNumErrors pg `shouldBe` 0
      it "counts only stale previews toward the render goal" $ \ctx -> do
        let config = ctxConfig ctx
            jpeg = jpegFile ctx
            orig = fileFullPath jpeg
        case autoSizeList ctx of
          [] -> expectationFailure "expected auto image sizes"
          (freshSize:staleSizes) -> do
            (repo, _) <- insertJpeg ctx jpeg
            writeNonEmpty (scaledImagePath config orig freshSize)
            pg <- forceBuildThumbCaches ctx repo
            pgNoop pg `shouldBe` 1
            pgGoal pg `shouldBe` length staleSizes
            pgDone pg `shouldBe` 0
            pgNumErrors pg `shouldBe` length staleSizes
            pgWork pg `shouldBe` length staleSizes
      it "reports each failed check and build once" $ \ctx -> do
        let config = ctxConfig ctx
            sizes = autoSizeList ctx
            names = ["a", "b"] :: [Text]
            picture parent name =
              mkImage config (ImageName (TS.fromText name)) "folder" Nothing Nothing
                [(jpegFile ctx) { fileName = TS.fromText name <> ".jpg", fileParent = parent }]
                Nothing [] [] Nothing MediaImage def
            withPictures parent = do
              repo <- getRepo ctx
              let pictures = map (picture parent) names
              forM_ pictures $ mapM_ (writeNonEmpty . fileFullPath) . imgJpegPath
              return repo { repoDirs = foldl' addImageToRepo Map.empty pictures }
            -- Which picture and size each error is about, from its
            -- item: the picture's name is in it, and the size ends it.
            about e = ( filter (\n -> ("/" <> n <> " ") `isInfixOf` peItem e) names
                      , Text.takeWhileEnd isDigit (peItem e) )
            -- In no particular order: none is defined for the errors.
            errors pg = sort $ map about (pgErrors pg)
            expected = sort [ ([n], tshow size) | n <- names, size <- sizes ]
        length sizes `shouldSatisfy` (> 1)
        -- Pictures outside the picture folders fail their check.
        outside <- withPictures (mkSym (pack $ cfgCacheDir config))
        checked <- forceBuildThumbCaches ctx outside
        errors checked `shouldBe` expected
        -- Files which aren't pictures fail to build.
        inside <- withPictures (fileParent (jpegFile ctx))
        built <- forceBuildThumbCaches ctx inside
        errors built `shouldBe` expected
      it "counts the images checked by the first pass" $ \ctx -> do
        let checkProgress = ctxRenderCheckProgress ctx
            readChecked = (\p -> (pgGoal p, pgDone p)) <$> readTVarIO checkProgress
        repo <- getRepo ctx
        -- A previous rendering's progress, e.g. an aborted one.
        atomically $ writeTVar checkProgress def { pgGoal = 5, pgDone = 2 }
        _ <- forceBuildThumbCaches ctx repo
        -- Nothing renderable: nothing to check.
        readChecked `shouldReturn` (0, 0)
        -- A picture outside the picture folders fails its check, but it
        -- is checked all the same.
        let jpeg = (jpegFile ctx) { fileParent = mkSym (pack . cfgCacheDir $ ctxConfig ctx) }
        writeFile (fileFullPath jpeg) ("" :: ByteString)
        (repo', _) <- insertJpeg ctx jpeg
        pg <- forceBuildThumbCaches ctx repo'
        pgNumErrors pg `shouldBe` length (autoSizeList ctx)
        readChecked `shouldReturn` (1, 1)
        -- Several images: counted as checked, the last one along with
        -- the rendering's goal.
        let config = ctxConfig ctx
            picture name = mkImage config name "folder" Nothing Nothing
                             [(jpegFile ctx) { fileName = unImageName name <> ".jpg" }]
                             Nothing [] [] Nothing MediaImage def
            dirs = foldl' addImageToRepo (repoDirs repo) (map picture ["x", "y", "z"])
        _ <- forceBuildThumbCaches ctx repo { repoDirs = dirs }
        readChecked `shouldReturn` (3, 3)
      it "records a fully cached image as noops with an empty work goal" $ \ctx -> do
        let config = ctxConfig ctx
            jpeg = jpegFile ctx
            orig = fileFullPath jpeg
            sizes = autoSizeList ctx
        (repo, _) <- insertJpeg ctx jpeg
        mapM_ (writeNonEmpty . scaledImagePath config orig) sizes
        pg <- forceBuildThumbCaches ctx repo
        pgNoop pg `shouldBe` length sizes
        pgGoal pg `shouldBe` 0
        pgDone pg `shouldBe` 0
        pgNumErrors pg `shouldBe` 0

    describe "getDirContents" $ do
      it "lists files and dirs and skips blacklisted names" $ \ctx -> do
        let config = ctxConfig ctx
            tmp = cfgCacheDir config </> "listing"
        createDirectoryIfMissing True (tmp </> ".thumbnails")
        createDirectoryIfMissing True (tmp </> "keep")
        touchFile (tmp </> "file.nef")
        touchFile (tmp </> ".thumbnails" </> "x")
        (dirs, files, skipped) <- getDirContents config tmp
        dirs `shouldMatchList` ["keep"]
        map inodeName files `shouldMatchList` ["file.nef"]
        dirs `shouldNotContain` [".", "..", ".thumbnails"]
        skipped `shouldBe` []
      it "skips symbolic links and special files" $ \ctx -> do
        let config = ctxConfig ctx
            tmp = cfgCacheDir config </> "listing"
        createDirectoryIfMissing True (tmp </> "dir")
        touchFile (tmp </> "file.nef")
        createSymbolicLink (tmp </> "file.nef") (tmp </> "link.jpg")
        createSymbolicLink (tmp </> "dir") (tmp </> "linkdir")
        createSymbolicLink "/nonexistent" (tmp </> "dangling.jpg")
        createNamedPipe (tmp </> "pipe.jpg") $
          ownerReadMode `unionFileModes` ownerWriteMode
        (dirs, files, skipped) <- getDirContents config tmp
        dirs `shouldMatchList` ["dir"]
        map inodeName files `shouldMatchList` ["file.nef"]
        skipped `shouldMatchList`
          [ ("link.jpg", "symbolic link, not followed")
          , ("linkdir", "symbolic link, not followed")
          , ("dangling.jpg", "symbolic link, not followed")
          , ("pipe.jpg", "not a regular file or directory, ignored")
          ]

    describe "recursiveScanPath" $ do
      it "records reverse directory stacks for nested files" $ \ctx -> do
        let config = ctxConfig ctx
            tmp = cfgCacheDir config </> "tree"
        touchFile (tmp </> "a.nef")
        touchFile (tmp </> "sub" </> "b.nef")
        (contents, skipped) <- recursiveScanPath config tmp []
        skipped `shouldBe` []
        let byName = Map.fromList [(inodeName ii, ii) | ii <- contents]
        inodeDirs (byName Map.! "a.nef") `shouldBe` []
        inodeDirs (byName Map.! "b.nef") `shouldBe` ["sub"]
        inodeFullName (byName Map.! "b.nef") `shouldBe` "sub/b.nef"
      it "returns nested skipped entries with their relative path" $ \ctx -> do
        let config = ctxConfig ctx
            tmp = cfgCacheDir config </> "tree"
        touchFile (tmp </> "sub" </> "b.nef")
        createSymbolicLink "/etc/passwd" (tmp </> "sub" </> "c.jpg")
        (contents, skipped) <- recursiveScanPath config tmp []
        map inodeFullName contents `shouldBe` ["sub/b.nef"]
        skipped `shouldBe` [("sub/c.jpg", "symbolic link, not followed")]

    describe "loadFolder" $ do
      it "classifies dummy files by extension" $ \ctx -> do
        let folder = sourceDir ctx </> "2024-01-01-trip"
        touchFile (folder </> "a.nef")
        touchFile (folder </> "notes.other")
        touchFile (folder </> "SHA1SUMS")
        pic <- loadFolder ctx "2024-01-01-trip" folder True
        imgStatus (pdImages pic Map.! "a") `shouldBe` ImageUnprocessed
        Map.keys (pdImages pic) `shouldBe` ["a"]
        map fileName (pdUntracked pic) `shouldMatchList` ["notes.other", "SHA1SUMS"]
      it "treats output jpegs as standalone" $ \ctx -> do
        let folder = outputDir ctx </> "2024-01-01-trip"
        touchFile (folder </> "b.jpg")
        pic <- loadFolder ctx "2024-01-01-trip" folder False
        imgStatus (pdImages pic Map.! "b") `shouldBe` ImageStandalone
      it "loads an explicit event from corydalis.yaml" $ \ctx -> do
        let folder = sourceDir ctx </> "2024-02-02-party"
        touchFile (folder </> "a.nef")
        writeFile (folder </> "corydalis.yaml") ("name: Birthday\nkind: birthday\n" :: ByteString)
        pic <- loadFolder ctx "2024-02-02-party" folder True
        pdEvent pic `shouldBe` Just BirthdayEvent
          { eventName = "Birthday"
          , eventPeople = []
          , eventSource = EventExplicit (Just (folder </> "corydalis.yaml"))
          }
        Map.notMember "corydalis" (pdImages pic) `shouldBe` True
        map fileName (pdUntracked pic) `shouldBe` ["corydalis.yaml"]
      describe "with an unusable corydalis.yaml" $ do
        let checkIgnored name setup = it name $ \ctx -> do
              let folder = sourceDir ctx </> "2024-05-05-yaml"
              touchFile (folder </> "a.nef")
              setup ctx folder :: IO ()
              pic <- loadFolder ctx "2024-05-05-yaml" folder True
              pdEvent pic `shouldBe` Nothing
            elsewhere ctx = outputDir ctx </> "elsewhere.yaml"
            yaml = "name: Birthday\nkind: birthday\n" :: ByteString
        checkIgnored "ignores, and reports, a symbolic link" $ \ctx folder -> do
          writeFile (elsewhere ctx) yaml
          createSymbolicLink (elsewhere ctx) (folder </> "corydalis.yaml")
        it "reports a symbolic link as a scan error" $ \ctx -> do
          let folder = sourceDir ctx </> "2024-05-05-yaml"
          touchFile (folder </> "a.nef")
          writeFile (elsewhere ctx) yaml
          createSymbolicLink (elsewhere ctx) (folder </> "corydalis.yaml")
          _ <- loadFolder ctx "2024-05-05-yaml" folder True
          errs <- map (peItem &&& peError) . pgErrors <$> getProgress ctx
          errs `shouldBe` [(pack $ folder </> "corydalis.yaml", "symbolic link, not followed")]
        checkIgnored "ignores a pipe, without blocking" $ \_ folder ->
          createNamedPipe (folder </> "corydalis.yaml") 0o600
        checkIgnored "ignores a file larger than the limit" $ \_ folder ->
          writeFile (folder </> "corydalis.yaml") $
            yaml <> BS.replicate maxYamlSize 0x20
        checkIgnored "ignores an invalid file" $ \_ folder ->
          writeFile (folder </> "corydalis.yaml") ("name: [unclosed\n" :: ByteString)
      it "ignores symbolic links and reports them as scan errors" $ \ctx -> do
        let folder = sourceDir ctx </> "2024-04-04-links"
        touchFile (folder </> "a.nef")
        createDirectoryIfMissing True (folder </> "sub")
        createSymbolicLink "/etc/passwd" (folder </> "key.jpg")
        createSymbolicLink "/etc/passwd" (folder </> "sub" </> "x.mp4")
        pic <- loadFolder ctx "2024-04-04-links" folder True
        Map.keys (pdImages pic) `shouldBe` ["a"]
        pdUntracked pic `shouldBe` []
        errs <- map (peItem &&& peError) . pgErrors <$> getProgress ctx
        errs `shouldMatchList`
          [ (pack $ folder </> "key.jpg", "symbolic link, not followed")
          , (pack $ folder </> "sub" </> "x.mp4", "symbolic link, not followed")
          ]
      it "attaches sidecars as orphaned when alone" $ \ctx -> do
        let folder = sourceDir ctx </> "2024-03-03-xmp"
        touchFile (folder </> "solo.xmp")
        pic <- loadFolder ctx "2024-03-03-xmp" folder True
        imgStatus (pdImages pic Map.! "solo") `shouldBe` ImageOrphaned

    describe "scanBaseDir" $ do
      it "loads date-named folders at any depth" $ \ctx -> do
        let root = sourceDir ctx
        touchFile (root </> "2024-01-01-top" </> "a.nef")
        touchFile (root </> "2024" </> "2024-01-02-year" </> "a.nef")
        touchFile (root </> "2024" </> "trips" </> "2024-01-03-deep" </> "a.nef")
        touchFile (root </> "2024" </> "not-a-date" </> "a.nef")
        touchFile (root </> "2024" </> "stray.nef")
        dirs <- scanBaseDir ctx root True
        map (pdName &&& pdMainPath) dirs `shouldMatchList`
          [ ("2024-01-01-top", fromString $ root </> "2024-01-01-top")
          , ("2024-01-02-year", fromString $ root </> "2024" </> "2024-01-02-year")
          , ("2024-01-03-deep", fromString $
                root </> "2024" </> "trips" </> "2024-01-03-deep")
          ]
        errs <- map peItem . pgErrors <$> getProgress ctx
        errs `shouldBe` []
      it "doesn't look for folders inside a folder" $ \ctx' -> do
        -- The unanchored regexes check that the nested directory's name
        -- doesn't mangle the image's name.
        let ctx = ctx' { ctxConfig = withUnanchoredNameRegexes (ctxConfig ctx') }
            folder = sourceDir ctx </> "2024" </> "2024-01-01-trip"
        touchFile (folder </> "a.nef")
        touchFile (folder </> "2024-01-02-day" </> "b.nef")
        dirs <- scanBaseDir ctx (sourceDir ctx) True
        map pdName dirs `shouldBe` ["2024-01-01-trip"]
        concatMap (Map.keys . pdImages) dirs `shouldMatchList`
          ["a", "2024-01-02-day/b"]
      it "stops at the maximum folder depth and reports it" $ \ctx' -> do
        let ctx = ctx' { ctxConfig = (ctxConfig ctx') { cfgMaxFolderDepth = 2 } }
            root = sourceDir ctx
        touchFile (root </> "a" </> "2024-01-01-ok" </> "a.nef")
        touchFile (root </> "a" </> "b" </> "2024-01-02-deep" </> "a.nef")
        dirs <- scanBaseDir ctx root True
        map pdName dirs `shouldBe` ["2024-01-01-ok"]
        errs <- map peItem . pgErrors <$> getProgress ctx
        errs `shouldBe` [pack $ root </> "a" </> "b"]
      it "looks for folders down to depth 4 by default" $ \ctx -> do
        let root = sourceDir ctx
        touchFile (root </> "a" </> "b" </> "c" </> "2024-01-01-ok" </> "a.nef")
        touchFile (root </> "a" </> "b" </> "c" </> "d" </> "2024-01-02-deep" </> "a.nef")
        dirs <- scanBaseDir ctx root True
        map pdName dirs `shouldBe` ["2024-01-01-ok"]
        errs <- map peItem . pgErrors <$> getProgress ctx
        errs `shouldBe` [pack $ root </> "a" </> "b" </> "c" </> "d"]
      it "names the setting's value for a directory too deep" $ \ctx' -> do
        let ctx = ctx' { ctxConfig = (ctxConfig ctx') { cfgMaxFolderDepth = 3 } }
            root = sourceDir ctx
        createDirectoryIfMissing True (root </> "a" </> "b" </> "c")
        _ <- scanBaseDir ctx root True
        errs <- map peError . pgErrors <$> getProgress ctx
        case errs of
          [e] -> do
            e `shouldSatisfy` ("maximum folder depth" `isInfixOf`)
            e `shouldSatisfy` ("3" `isInfixOf`)
          _ -> expectationFailure $ "Expected one error, got: " ++ show errs
      it "doesn't search or report blacklisted directories" $ \ctx' -> do
        let ctx = ctx' { ctxConfig = (ctxConfig ctx') { cfgMaxFolderDepth = 2 } }
            root = sourceDir ctx
        touchFile (root </> ".thumbnails" </> "2024-01-01-x" </> "a.nef")
        touchFile (root </> "2024" </> ".thumbnails" </> "a.nef")
        dirs <- scanBaseDir ctx root True
        map pdName dirs `shouldBe` []
        errs <- map peItem . pgErrors <$> getProgress ctx
        errs `shouldBe` []
      it "reports symbolic links to folders as scan errors" $ \ctx -> do
        let level1 = sourceDir ctx </> "level1"
        touchFile (level1 </> "2024-01-01-trip" </> "a.nef")
        createSymbolicLink (level1 </> "2024-01-01-trip")
          (level1 </> "2024-01-02-link")
        dirs <- scanBaseDir ctx (sourceDir ctx) True
        map pdName dirs `shouldMatchList` ["2024-01-01-trip"]
        errs <- map (peItem &&& peError) . pgErrors <$> getProgress ctx
        errs `shouldBe`
          [(pack $ level1 </> "2024-01-02-link", "symbolic link, not followed")]
      it "merges source raw and output jpeg of the same image" $ \ctx -> do
        let config = ctxConfig ctx
            rawRoot = sourceDir ctx
            jpgRoot = outputDir ctx
        touchFile (rawRoot </> "level1" </> "2024-01-01-trip" </> "a.nef")
        touchFile (jpgRoot </> "level1" </> "2024-01-01-trip" </> "a.jpg")
        rawPics <- scanBaseDir ctx rawRoot True
        jpgPics <- scanBaseDir ctx jpgRoot False
        let merged = foldl' (flip (addDirToRepo config)) Map.empty (rawPics ++ jpgPics)
            img = pdImages (merged Map.! "2024-01-01-trip") Map.! "a"
        imgStatus img `shouldBe` ImageProcessed
        isJust (imgRawPath img) `shouldBe` True
        null (imgJpegPath img) `shouldBe` False
      it "merges folders found at different depths" $ \ctx -> do
        let config = ctxConfig ctx
        touchFile (sourceDir ctx </> "2024" </> "trips" </> "2024-01-01-trip" </> "a.nef")
        touchFile (outputDir ctx </> "2024-01-01-trip" </> "a.jpg")
        rawPics <- scanBaseDir ctx (sourceDir ctx) True
        jpgPics <- scanBaseDir ctx (outputDir ctx) False
        let merged = foldl' (flip (addDirToRepo config)) Map.empty (rawPics ++ jpgPics)
        Map.keys merged `shouldBe` ["2024-01-01-trip"]
        imgStatus (pdImages (merged Map.! "2024-01-01-trip") Map.! "a")
          `shouldBe` ImageProcessed

    describe "countDir" $ do
      it "counts what the scan finds" $ \ctx' -> do
        let ctx = ctx' { ctxConfig = (ctxConfig ctx') { cfgMaxFolderDepth = 3 } }
            config = ctxConfig ctx
            root = sourceDir ctx
        touchFile (root </> "2024-01-01-top" </> "a.nef")
        touchFile (root </> "2024-01-01-top" </> "sub" </> "b.nef")
        touchFile (root </> "2024" </> "2024-01-02-year" </> "a.nef")
        touchFile (root </> "2024" </> "x" </> "2024-01-03-deep" </> "a.nef")
        touchFile (root </> "2024" </> "x" </> "too-deep" </> "a.nef")
        touchFile (root </> "2024" </> "stray.nef")
        createSymbolicLink "/etc/passwd" (root </> "2024" </> "link")
        items <- countDir config root
        _ <- scanBaseDir ctx root True
        pg <- getProgress ctx
        -- The four files in folders, the link and the too deep directory.
        items `shouldBe` 6
        pgNoop pg + pgDone pg + length (pgErrors pg) `shouldBe` items
