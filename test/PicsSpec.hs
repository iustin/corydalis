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

module PicsSpec (spec) where

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Default
import           Data.Either      (isLeft, isRight)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import qualified Data.Text.Short  as TS
import           Data.Time        (LocalTime (..), ZonedTime (..), getZonedTime,
                                   midnight, utc,
                                   zonedTimeToLocalTime)
import qualified Control.Exception
import           Control.Exception (AsyncException (ThreadKilled))
import           Data.Time.Clock.POSIX (POSIXTime)
import           Control.Concurrent.STM.TSem (signalTSem, waitTSem)
import           Data.Char        (isDigit)
import qualified Data.Text        as Text
import           System.Directory (createDirectoryIfMissing,
                                   doesDirectoryExist, doesFileExist,
                                   doesPathExist,
                                   listDirectory, renameFile)
import           System.Exit      (ExitCode (..))
import           System.FilePath  (pathSeparator, splitFileName,
                                   takeDirectory,
                                   takeFileName)
import           System.Posix.Files (FileStatus, createNamedPipe,
                                     createSymbolicLink,
                                     getSymbolicLinkStatus,
                                     ownerReadMode, ownerWriteMode,
                                     unionFileModes)
import           Control.Monad.Logger (LogLevel (..))
import           System.Log.FastLogger (fromLogStr)
import           System.Posix.IO   (OpenMode (..), closeFd, defaultFileFlags,
                                   openFd)
import qualified System.IO
import           System.Process.Typed (proc, readProcess, setCloseFds)

import           AtomTypes
import           Cache              (cacheTempDir, cachedBasename)
import           Exif
import           Pics
import           Utils.Storage      (resolveStorageDirs)
import           Utils.Tools        (decodeToolOutput, readToolProcess)
import           Utils.Yaml         (maxYamlSize)
import           TestImport

-- | A modification time far in the future (in 2096), newer than any test
-- file, for cache files to always be outdated. After that year, the
-- tests using it will fail and it will need to be updated.
farFutureTime :: POSIXTime
farFutureTime = 4000000000

parentSym :: SymbolizedItem
parentSym = mkSym "/pics/folder"

runLoad :: Config -> Bool -> InodeInfo -> (Image, [Image])
runLoad config isSource =
  loadImage config "folder" parentSym isSource Map.empty

runLoadExif :: Config -> Bool -> InodeInfo -> Map Text EExif -> (Image, [Image])
runLoadExif config isSource ii cache =
  loadImage config "folder" parentSym isSource cache ii

picDirWith :: ShortText -> [Image] -> PicDir
picDirWith name images =
  let imgMap = Map.fromList [(imgName i, i) | i <- images]
  in (createTestPicDir name)
       { pdImages = imgMap
       , pdTimeSort = buildTimeSort imgMap
       , pdStats = computeImagesStats imgMap
       , pdExif = buildGroupExif imgMap
       }

fileSized :: Text -> FileOffset -> File
fileSized name sz = (simpleFile name) { fileSize = sz }

withProdNameRegexes :: Config -> Config
withProdNameRegexes config =
  config
    { cfgRangeRegex = fromMaybe (cfgRangeRegex config) $
        mkRegex "^(.*_)([0-9]+)-([0-9]+)$"
    , cfgCopyRegex = fromMaybe (cfgCopyRegex config) $
        mkRegex "^(.*)(-(close_view|copy_|publish|published|aspect_change))$"
    }

-- | Uses unanchored copy and range regexes (as in older sample
-- settings), which also match across directory names: these check that
-- only the file name is matched.
withUnanchoredNameRegexes :: Config -> Config
withUnanchoredNameRegexes config =
  config
    { cfgRangeRegex = fromMaybe (error "Invalid range regex") $
        mkRegex "(.*_)([0-9]+)-([0-9]+)"
    , cfgCopyRegex = fromMaybe (error "Invalid copy regex") $
        mkRegex "(.*)(-([0-9]|Edit))"
    }

datedExif :: Integer -> Int -> Int -> Exif
datedExif y m d =
  def { exifCreateDate = Just (ExifTime (ZonedTime (LocalTime (fromGregorian y m d) midnight) utc)) }

isOwnershipAbort :: SomeException -> Bool
isOwnershipAbort e = "ownership changed" `isInfixOf` show e

sourceDir :: Ctx -> FilePath
sourceDir ctx =
  fromMaybe (error "No source directory") (headMay $ cfgSourceDirs $ ctxConfig ctx)

outputDir :: Ctx -> FilePath
outputDir ctx =
  fromMaybe (error "No output directory") (headMay $ cfgOutputDirs $ ctxConfig ctx)

jpegFile :: Ctx -> File
jpegFile ctx =
  (simpleFile "a.jpg")
    { fileParent = mkSym (pack $ outputDir ctx)
    }

insertJpeg :: Ctx -> File -> IO (Repository, Image)
insertJpeg ctx jpeg = do
  repo <- getRepo ctx
  let config = ctxConfig ctx
      img = mkImage config "a" "folder" Nothing Nothing [jpeg] Nothing [] [] Nothing MediaImage def
      dirs = addImageToRepo config (repoDirs repo) img
      repo' = repo { repoDirs = dirs
                   , repoStats = computeRepoStats dirs
                   , repoExif = repoGlobalExif dirs
                   }
  atomically $ writeTVar (ctxRepo ctx) repo'
  return (repo', img)

autoSizeList :: Ctx -> [Int]
autoSizeList = Set.toList . cfgAutoImageSizes . ctxConfig

-- | A small, valid SVG image.
--
-- It is harmless, and would be converted successfully if decoded as an
-- SVG; real attacks would instead reference other files.
svgDocument :: ByteString
svgDocument =
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"10\" height=\"10\">\n\
  \<rect x=\"0\" y=\"0\" width=\"10\" height=\"10\" fill=\"red\"/>\n\
  \</svg>\n"

-- | A small, harmless PostScript program.
--
-- As for the SVG, it would be converted successfully if decoded as such.
postScriptDocument :: ByteString
postScriptDocument =
  "%!PS-Adobe-3.0\n\
  \/Times-Roman findfont 20 scalefont setfont\n\
  \10 10 moveto (harmless) show\n\
  \showpage\n"

-- | A time limit for tests that could block if they fail, so that they
-- fail instead of blocking the test suite: only a last resort, not how
-- the tests synchronise.
lastResort :: Int
lastResort = 30 * 1000000

-- | Runs convert to create a test image, without the application's
-- policy, which doesn't allow e.g. creating images from a colour.
convertTestImage :: [String] -> IO ()
convertTestImage args = do
  (code, _, err) <- readProcess (proc "convert" args)
  unless (code == ExitSuccess) $
    expectationFailure ("Failed to create test image: " ++ show err)

-- | Writes a real JPEG image, using ImageMagick.
writeRealJpeg :: FilePath -> IO ()
writeRealJpeg path = convertTestImage ["-size", "8x8", "xc:white", "jpeg:" ++ path]

-- | Writes a JPEG image large enough to be accepted as an embedded one.
writePreviewJpeg :: FilePath -> IO ()
writePreviewJpeg path =
  convertTestImage ["-size", "128x128", "gradient:red-blue", "jpeg:" ++ path]

-- | Writes a JPEG image as 'writePreviewJpeg' does, under a name that
-- convert might not use as given (e.g. one with format specifiers, in
-- the file's name or a folder's), creating its directory if needed:
-- convert writes it to its standard output, not to a named file.
writePreviewJpegAs :: FilePath -> IO ()
writePreviewJpegAs path = do
  (code, out, err) <- readProcess (proc "convert" ["-size", "128x128", "gradient:red-blue",
                                                   "jpeg:-"])
  unless (code == ExitSuccess) $
    expectationFailure ("Failed to create test image: " ++ show err)
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path (toStrict out)

-- | Returns a property of an image, as formatted by convert (without the
-- application's policy).
imageProperty :: FilePath -> String -> IO String
imageProperty path format = do
  (code, out, err) <- readProcess (proc "convert" [path, "-format", format, "info:"])
  unless (code == ExitSuccess) $
    expectationFailure ("Failed to read " ++ format ++ " of " ++ path ++ ": " ++ show err)
  return . unpack $ (decodeUtf8 (toStrict out) :: Text)

-- | Whether a single-image file is red, judging by its centre (as a
-- thumbnail can be padded).
isRedImage :: FilePath -> IO Bool
isRedImage path =
  (== "1") <$> imageProperty path
    "%[fx:p{w/2,h/2}.r>0.8&&p{w/2,h/2}.g<0.2&&p{w/2,h/2}.b<0.2]"

-- | Writes a file with the given embedded preview image.
--
-- This uses MIE (Meta Information Encapsulation), a metadata container
-- format designed by the exiftool author. It is not a camera format,
-- and Corydalis never sees such files in a library, but exiftool can
-- create one from scratch, including the preview image; in the real
-- formats (JPEG, NEF, DNG, etc.), exiftool can only update an existing
-- preview, so the tests would need real camera files instead. The file
-- extension doesn't matter, since the tests extract the preview
-- directly, not via a scan.
--
-- Exiftool checks that MIE previews are JPEGs, so this can't create a
-- file with a non-JPEG preview; that case is only tested via
-- 'checkEmbeddedImage'.
writeFileWithPreview :: FilePath -> FilePath -> IO ()
writeFileWithPreview preview path = do
  (code, _, err) <- readProcess (proc "exiftool" ["-q", "-o", path,
                                                  "-PreviewImage<=" ++ preview, preview])
  unless (code == ExitSuccess) $
    expectationFailure ("Failed to create test file with preview: " ++ show err)

-- | Creates a (non-empty) directory where an output file is expected,
-- so that the file can't be written (moved) there.
blockOutput :: FilePath -> IO ()
blockOutput path = do
  createDirectoryIfMissing True path
  writeFile (path </> "file") ("" :: ByteString)

-- | A file name longer than allowed (255 bytes, on most file systems).
tooLongName :: FilePath
tooLongName = replicate 300 'x'

-- | Makes the cache directory of a file a regular file, so that its
-- cache files can't be checked or written.
blockCacheDir :: Config -> FilePath -> IO ()
blockCacheDir config path = do
  let dir = takeDirectory (embeddedImagePath config path)
  createDirectoryIfMissing True (takeDirectory dir)
  writeFile dir ("" :: ByteString)

-- | Writes a short movie, using ffmpeg.
writeMovie :: FilePath -> IO ()
writeMovie path = do
  (code, _, err) <- readProcess (proc "ffmpeg" ["-nostdin", "-loglevel", "error",
                                                "-f", "lavfi",
                                                "-i", "testsrc=duration=1:size=64x64:rate=1",
                                                "-c:v", "mpeg4", path])
  unless (code == ExitSuccess) $
    expectationFailure ("Failed to create test movie: " ++ show err)

-- | Builds the thumbnail of a file, returning the result.
buildThumbnail :: Config -> FilePath -> Either Text String
               -> IO (Either ImageError (Bool, MimeType, FilePath))
buildThumbnail config path decoder =
  try $ testToolLimits >>= \tools -> loadCachedOrBuild tools config path path decoder "image/jpeg" 0
          (ImageSize (cfgThumbnailSize config))

-- | Checks that building the thumbnail failed without creating it.
shouldFailToBuild :: Config -> FilePath
                  -> Either ImageError (Bool, MimeType, FilePath) -> IO ()
shouldFailToBuild config path result = do
  result `shouldSatisfy` isLeft
  doesFileExist (scaledImagePath config path (cfgThumbnailSize config))
    `shouldReturn` False

-- | A preview size that is not a thumbnail (so the preview is a JPEG).
previewSize :: ImageSize
previewSize = ImageSize 1920

-- | Builds a (non-thumbnail) preview of a JPEG file, returning the result.
buildPreview :: Config -> FilePath
             -> IO (Either ImageError (Bool, MimeType, FilePath))
buildPreview config path = buildPreviewOf config path "jpeg"

-- | Builds a (non-thumbnail) preview of a file, with the given decoder,
-- returning the result.
buildPreviewOf :: Config -> FilePath -> String
               -> IO (Either ImageError (Bool, MimeType, FilePath))
buildPreviewOf config path decoder =
  try $ testToolLimits >>= \tools -> loadCachedOrBuild tools config path path (Right decoder) "image/jpeg" 0 previewSize

-- | Checks that a preview was built, as the only file in its directory
-- (so with no other output or temporary files next to it), and returns
-- its contents. The picture must be the only one with cache files in
-- that directory.
shouldBeOnlyPreview :: Either ImageError (Bool, MimeType, FilePath) -> FilePath
                    -> IO LByteString
shouldBeOnlyPreview result expected = do
  case result of
    Right (built, _, path) -> (built, path) `shouldBe` (True, expected)
    Left e -> expectationFailure ("Building the preview failed: " ++ show e)
  listDirectory (takeDirectory expected) `shouldReturn` [takeFileName expected]
  fromStrict <$> readFile expected

-- | The path of the preview built by 'buildPreview'.
previewPath :: Config -> FilePath -> IO FilePath
previewPath config path =
  case findBestSize previewSize (cfgAllImageSizes config) of
    Nothing  -> expectationFailure "expected a cached size for 1920" >> return ""
    Just res -> return $ scaledImagePath config path res

-- | Checks that the (non-thumbnail) preview of a picture was built, as a
-- JPEG and alone in its directory (see 'shouldBeOnlyPreview'), and
-- returns its path.
shouldBeOnlyPreviewOf :: Config -> FilePath
                      -> Either ImageError (Bool, MimeType, FilePath)
                      -> IO FilePath
shouldBeOnlyPreviewOf config path result = do
  expected <- previewPath config path
  preview <- shouldBeOnlyPreview result expected
  jpegMagic `BSL.isPrefixOf` preview `shouldBe` True
  return expected

-- | Writes a non-empty file, creating its directory if needed.
writeNonEmpty :: FilePath -> IO ()
writeNonEmpty path = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path ("x" :: ByteString)

-- | The coders that the shipped ImageMagick policy allows reading.
--
-- This is a simple text scan of the coder rules granting read rights,
-- which is all the shipped policy uses.
policyReadableCoders :: IO [Text]
policyReadableCoders = do
  policy <- readFileUtf8 "config/magick/policy.xml"
  return [ coder
         | line <- lines policy
         , "domain=\"coder\"" `isInfixOf` line
         , "rights=\"read" `isInfixOf` line
         , let coders = takeWhile (/= '}') . drop 1 . dropWhile (/= '{') $ line
         , coder <- splitElem ',' coders
         ]

-- | A file for which exiftool reported the given MIME type.
fileWithMimeType :: Maybe Text -> File
fileWithMimeType mime =
  (simpleFile "a.jpg") { fileExif = def { exifMimeType = mkSym <$> mime } }

-- | 'extractEmbedded', with the default tool limits.
extractEmbeddedT :: Config -> FilePath -> POSIXTime -> String
                 -> IO (Either Text (MimeType, FilePath))
extractEmbeddedT config path mtime tag = do
  tools <- testToolLimits
  extractEmbedded tools config path mtime tag

-- | 'extractFirstFrame', with the default tool limits.
extractFirstFrameT :: Config -> FilePath -> POSIXTime
                   -> IO (Either Text (MimeType, FilePath))
extractFirstFrameT config path mtime = do
  tools <- testToolLimits
  extractFirstFrame tools config path mtime

-- | Runs something which runs a tool (given the limits to run it
-- within), checking that it waits for a tool slot before creating any
-- file: neither the given cache file's directory, nor a temporary file.
-- Returns its result, once given the slot.
waitsForSlotFirst :: Config -> FilePath -> (ToolLimits -> IO a)
                  -> IO (Either SomeException a)
waitsForSlotFirst config target run = do
  tools <- atomically $ newToolLimits 1 defaultRenderToolTimeout
  -- Taking the only slot, as a running tool would.
  atomically . waitTSem $ tlSlots tools
  -- Stopped if the test fails before it finishes (e.g. while it waits
  -- for the slot, which would then never be freed).
  withAsync (run tools) $ \running -> do
    -- It does nothing else in STM, so it is then waiting for the slot.
    waitUntilBlockedInSTM (asyncThreadId running)
    doesDirectoryExist (takeDirectory target) `shouldReturn` False
    tmpFiles <- doesDirectoryExist (cacheTempDir config) >>= \case
      True -> listDirectory (cacheTempDir config)
      False -> return []
    tmpFiles `shouldBe` []
    atomically . signalTSem $ tlSlots tools
    -- With a last resort time limit, in case it never finishes.
    timeout lastResort (waitCatch running) >>=
      maybe (expectationFailure "The run never finished" >> error "unreachable") return

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
  describe "inode info" $ do
    it "works without directories" $ \_ -> do
      let ii = InodeInfo "file.jpg" [] False 0 0 0
      inodeFullName ii `shouldBe` "file.jpg"
    it "returns the full path for an inode" $ \_ -> do
      let ii = InodeInfo "file.jpg" ["subdir", "dir"] False 0 0 0
      inodeFullName ii `shouldBe` "dir/subdir/file.jpg"
  describe "file info" $ do
    it "empty parents return the file name" $ \_ -> do
      let f = File { fileName = "file.jpg", fileCTime = 0, fileMTime = 0, fileSize = 0, fileParent = mkSym "", fileDirs = mkSym "", fileExif = def }
      fileFullPath f `shouldBe` "file.jpg"
      fileRelPath f `shouldBe` "file.jpg"
    it "returns the full path for a file" $ \_ -> do
      let f = File { fileName = "file.jpg", fileCTime = 0, fileMTime = 0, fileSize = 0, fileDirs = mkSym "", fileParent = mkSym "/pics/2022", fileExif = def }
      fileFullPath f `shouldBe` "/pics/2022/file.jpg"
      fileRelPath f `shouldBe` "file.jpg"
    it "returns the full path for a file with dirs" $ \_ -> do
      let f = File { fileName = "file.jpg", fileCTime = 0, fileMTime = 0, fileSize = 0, fileDirs = mkSym "dir/subdir", fileParent = mkSym "/pics/2022", fileExif = def }
      fileFullPath f `shouldBe` "/pics/2022/dir/subdir/file.jpg"
      fileRelPath f `shouldBe` "dir/subdir/file.jpg"
  describe "file MIME type" $ do
    forM_ ["image/jpeg", "image/png", "image/avif", "image/x-canon-cr2",
           "video/mp4", "video/quicktime"] $ \mime ->
      it ("uses the type reported by exiftool for " ++ show mime) $ \_ ->
        fileMimeType "image/jpeg" (fileWithMimeType (Just mime)) `shouldBe` mime
    -- Types that browsers treat as documents, malformed types, and header
    -- injection attempts all get the default.
    forM_ ["image/svg+xml", "IMAGE/SVG+XML", "text/html", "text/plain",
           "application/xhtml+xml", "application/pdf", "text/xml", "",
           "image/jpeg; charset=utf-8", "image/jpeg\r\nX-Injected: 1"] $ \mime ->
      it ("uses the default instead of " ++ show mime) $ \_ ->
        fileMimeType "image/jpeg" (fileWithMimeType (Just mime)) `shouldBe` "image/jpeg"
    it "uses the default without a reported type" $ \_ ->
      fileMimeType "video/mp4" (fileWithMimeType Nothing) `shouldBe` "video/mp4"
  describe "image decoders" $ withConfig $ do
    forM_ [ ("a.jpg", "jpeg"), ("a.JPG", "jpeg"), ("dir/a.Jpeg", "jpeg")
          , ("a.heic", "heic"), ("a.tif", "tiff"), ("a.png", "png")] $ \(path, decoder) ->
      it ("finds the decoder for " ++ show path) $ \config ->
        imageDecoder config path `shouldBe` Right decoder
    forM_ ["a.svg", "a.ps", "a.nef", "a", "a.jpg.svg"] $ \path ->
      it ("has no decoder for " ++ show path) $ \config ->
        imageDecoder config path `shouldSatisfy` isLeft
    it "uses the configured decoders" $ \config -> do
      let config' = config { cfgImageDecoders = mapFromList [("dng", "dng")] }
      imageDecoder config' "a.DNG" `shouldBe` Right "dng"
      imageDecoder config' "a.jpg" `shouldSatisfy` isLeft
    it "names the extension when no decoder is found" $ \config ->
      imageDecoder config "a.xyz" `shouldBeLeftWithMessage` "'xyz'"
  describe "building previews" $ withConfig $ do
    it "doesn't give convert the application's open files" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "photo.jpg"
          succeeded = either (const False) (\(c, _, _) -> c == ExitSuccess)
      writePreviewJpeg path
      bracket (openFd path ReadOnly defaultFileFlags) closeFd $ \fd -> do
        pconfig <- convertProcess config ["jpeg:/dev/fd/" ++ show fd, "png:" ++ path ++ ".png"]
        -- The file can be read by convert if it gets it.
        readToolProcess (setCloseFds False pconfig) >>= (`shouldSatisfy` succeeded)
        readToolProcess pconfig >>= (`shouldSatisfy` not . succeeded)
    it "builds the thumbnail of a JPEG" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "photo.JPG"
      writeRealJpeg path
      result <- buildThumbnail config path (imageDecoder config path)
      case result of
        Left e -> expectationFailure ("Failed to build thumbnail: " ++ show e)
        Right (built, mime, thumb) -> do
          built `shouldBe` True
          mime `shouldBe` "image/png"
          doesFileExist thumb `shouldReturn` True
    forM_ [("SVG", svgDocument), ("PostScript", postScriptDocument)] $ \(kind, contents) ->
      it ("does not decode a " ++ kind ++ " document named as a JPEG") $ \config -> do
        requireConvert
        let path = cfgCacheDir config </> "photo.jpg"
        writeFile path contents
        buildThumbnail config path (imageDecoder config path) >>=
          shouldFailToBuild config path
    it "does not run convert without a decoder" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "photo.jpg"
      writeRealJpeg path
      result <- buildThumbnail config path (Left "no decoder")
      shouldFailToBuild config path result
      case result of
        Left (ImageError msg) -> msg `shouldBe` "no decoder"
        _                     -> expectationFailure ("Unexpected result: " ++ show result)
    it "reads the first image from the standard input, and writes to the standard output" $ \_ ->
      forM_ [(PreviewScaled, "jpeg:"), (PreviewThumbnail, "png:")] $ \(kind, coder) -> do
        let args = previewArgs kind "tiff" 800
        headMay args `shouldBe` Just "tiff:/dev/fd/0[0]"
        lastMay args `shouldBe` Just (coder ++ "/dev/fd/1")
        -- Never enlarged.
        args `shouldSatisfy` elem "800x800>"
    it "removes the picture's path from thumbnails" $ \_ -> do
      let pairs xs = zip xs (drop 1 xs)
          thumbnail = previewArgs PreviewThumbnail "jpeg" 64
          scaled = previewArgs PreviewScaled "jpeg" 800
      -- After -thumbnail, which records it.
      dropWhile (/= "-thumbnail") thumbnail `shouldSatisfy` (("+set", "Thumb::URI") `elem`) . pairs
      scaled `shouldSatisfy` notElem "-thumbnail"
    it "accepts previews of the expected format" $ \_ -> do
      checkPreviewImage PreviewScaled jpegMagic `shouldBe` Right ()
      checkPreviewImage PreviewThumbnail pngMagic `shouldBe` Right ()
    it "refuses empty previews" $ \_ -> do
      checkPreviewImage PreviewScaled "" `shouldSatisfy` isLeft
      checkPreviewImage PreviewThumbnail "" `shouldSatisfy` isLeft
    it "refuses previews of another format" $ \_ -> do
      checkPreviewImage PreviewScaled pngMagic `shouldBeLeftWithMessage` "JPEG"
      checkPreviewImage PreviewThumbnail jpegMagic `shouldBeLeftWithMessage` "PNG"
    it "refuses previews with only part of the signature" $ \_ -> do
      checkPreviewImage PreviewScaled (BSL.take 2 jpegMagic) `shouldSatisfy` isLeft
      checkPreviewImage PreviewThumbnail (BSL.take 4 pngMagic) `shouldSatisfy` isLeft
    it "checks the preview file written by convert" $ \config -> do
      let path = cfgCacheDir config </> "preview"
          check kind contents = do
            writeFile path (toStrict contents)
            checkPreviewFile kind path
      check PreviewScaled (jpegMagic <> "rest") `shouldReturn` Right ()
      check PreviewThumbnail (pngMagic <> "rest") `shouldReturn` Right ()
      check PreviewScaled "" >>= (`shouldSatisfy` isLeft)
      check PreviewScaled (BSL.take 2 jpegMagic) >>= (`shouldSatisfy` isLeft)
      check PreviewScaled (pngMagic <> "rest") >>= (`shouldSatisfy` isLeft)
      check PreviewThumbnail (jpegMagic <> "rest") >>= (`shouldSatisfy` isLeft)
    describe "with a stand-in for convert" $ do
      -- A shell script, which gets the picture as its standard input,
      -- and writes the preview to its standard output.
      let build config script = do
            tools <- testToolLimits
            let target = cfgCacheDir config </> "dir" </> "preview"
                picture = cfgCacheDir config </> "picture"
            writeFile picture ("the picture" :: ByteString)
            result <- try . buildPreviewFile tools config PreviewScaled target picture $
                        proc "sh" ["-c", script]
            return (target, result :: Either ImageError ())
          failsWith config script check = do
            (target, result) <- build config script
            case result of
              Left (ImageError msg) -> msg `shouldSatisfy` check
              _ -> expectationFailure ("Unexpected result: " ++ show result)
            -- No file is left behind either: neither the cache file nor
            -- the temporary one.
            listDirectory (takeDirectory target) `shouldReturn` []
            listDirectory (cacheTempDir config) `shouldReturn` []
      it "caches the preview, if it is of the expected format" $ \config -> do
        (target, result) <- build config "printf '\\377\\330\\377rest'"
        result `shouldSatisfy` isRight
        readFile target `shouldReturn` "\255\216\255rest"
        listDirectory (takeDirectory target) `shouldReturn` [takeFileName target]
      it "gives the picture as the standard input" $ \config -> do
        (target, result) <- build config "printf '\\377\\330\\377'; cat"
        result `shouldSatisfy` isRight
        readFile target `shouldReturn` "\255\216\255the picture"
      it "refuses to cache an empty preview, with the tool's messages" $ \config ->
        failsWith config "echo 'wrote it elsewhere' >&2"
          (\msg -> "wrote it elsewhere" `isInfixOf` msg && not ("\n" `isSuffixOf` msg))
      it "refuses to cache a preview of another format" $ \config ->
        failsWith config "printf GIF89a" ("JPEG" `isInfixOf`)
      it "names the exit code of a failed tool" $ \config ->
        failsWith config "exit 3" (tshow (ExitFailure 3) `isInfixOf`)
      it "refuses a preview that looks right, from a failed tool" $ \config ->
        failsWith config "printf '\\377\\330\\377partial'; exit 1"
          (tshow (ExitFailure 1) `isInfixOf`)
      it "refuses a picture that isn't a regular file, without waiting" $ \config -> do
        tools <- testToolLimits
        let target = cfgCacheDir config </> "dir" </> "preview"
            picture = cfgCacheDir config </> "pipe"
        -- Opening a pipe for reading would wait for a writer.
        createNamedPipe picture 0o600
        -- The time limit is only a last resort, so that a regression
        -- fails the test instead of blocking the whole test suite.
        result <- timeout lastResort . try . buildPreviewFile tools config PreviewScaled target picture $
                    proc "sh" ["-c", "cat"]
        case result of
          Just (Left (ImageError msg)) -> msg `shouldSatisfy` (pack picture `isInfixOf`)
          Nothing -> expectationFailure "Building the preview blocked"
          _ -> expectationFailure ("Unexpected result: " ++ show result)
        listDirectory (takeDirectory target) `shouldReturn` []
      it "lets a cancellation through, stopping the tool" $ \config -> do
        tools <- testToolLimits
        let target = cfgCacheDir config </> "dir" </> "preview"
            picture = cfgCacheDir config </> "picture"
            started = cfgCacheDir config </> "started"
        writeFile picture ("the picture" :: ByteString)
        -- The tool says it started through a pipe, then waits; opened
        -- for both reading and writing, the pipe doesn't wait for the
        -- tool.
        createNamedPipe started 0o600
        withBinaryFile started ReadWriteMode $ \h -> do
          -- Also stopped if the test fails before cancelling it.
          withAsync (buildPreviewFile tools config PreviewScaled target picture $
                       proc "sh" ["-c", "echo started > \"$0\"; exec sleep 1000", started]) $
            \running -> do
              -- With a last resort time limit, in case the tool never runs.
              timeout lastResort (System.IO.hGetLine h) `shouldReturn` Just "started"
              cancel running
              waitCatch running >>= \case
                Left e -> (fromException e :: Maybe ImageError) `shouldSatisfy` isNothing
                Right () -> expectationFailure "The build wasn't cancelled"
        -- No file is left behind: neither the cache file nor the
        -- temporary one.
        listDirectory (takeDirectory target) `shouldReturn` []
        listDirectory (cacheTempDir config) `shouldReturn` []
      it "waits for a slot before creating any file" $ \config -> do
        let target = cfgCacheDir config </> "dir" </> "preview"
            picture = cfgCacheDir config </> "picture"
        writeFile picture ("the picture" :: ByteString)
        result <- waitsForSlotFirst config target $ \tools ->
          buildPreviewFile tools config PreviewScaled target picture $
            proc "sh" ["-c", "printf '\\377\\330\\377rest'"]
        result `shouldSatisfy` isRight
        readFile target `shouldReturn` "\255\216\255rest"
      it "names a picture that can't be read" $ \config -> do
        tools <- testToolLimits
        let target = cfgCacheDir config </> "dir" </> "preview"
            picture = cfgCacheDir config </> "missing"
        result <- try . buildPreviewFile tools config PreviewScaled target picture $
                    proc "sh" ["-c", "cat"]
        case result of
          Left (ImageError msg) -> msg `shouldSatisfy` (pack picture `isInfixOf`)
          _ -> expectationFailure ("Unexpected result: " ++ show result)
    describe "tryImageError" $ do
      it "returns the message of an image error" $ \_ ->
        tryImageError (throwIO (ImageError "the message")) `shouldReturn` Left "the message"
      it "returns other exceptions, shown" $ \_ ->
        tryImageError (throwIO (userError "disk on fire")) >>= (`shouldBeLeftWithMessage` "disk on fire")
      it "returns success" $ \_ ->
        tryImageError (return ()) `shouldReturn` Right ()
      it "doesn't catch asynchronous exceptions" $ \_ ->
        -- Thrown as is: UnliftIO's throwIO would make it synchronous.
        tryImageError (Control.Exception.throwIO ThreadKilled) `shouldThrow` (== ThreadKilled)
    forM_ ["jpeg", "png", "tiff", "webp", "heic"] $ \decoder ->
      it ("builds the preview of a " ++ decoder ++ " image") $ \config -> do
        requireConvert
        when (decoder == "heic") requireHeic
        let path = cfgCacheDir config </> "picture"
        convertTestImage ["-size", "64x64", "gradient:red-blue", decoder ++ ":" ++ path]
        buildPreviewOf config path decoder >>= void . shouldBeOnlyPreviewOf config path
    -- Some formats with several images that convert can write. libheif
    -- makes the first image it writes the primary one, so this only
    -- checks that the first image of a HEIC file is used; that
    -- ImageMagick returns the primary image first, even when it isn't
    -- the first one, is not tested.
    forM_ [ ("tiff", "the pages of a TIFF")
          , ("webp", "the frames of a WebP")
          , ("heic", "the images of a HEIC file")
          ] $ \(decoder, what) ->
      it ("builds the preview from the first of " ++ what) $ \config -> do
        requireConvert
        when (decoder == "heic") requireHeic
        let path = cfgCacheDir config </> "picture"
            frame colour = ["-size", "64x64", "xc:" ++ colour]
        convertTestImage (frame "red" ++ frame "blue" ++ [decoder ++ ":" ++ path])
        preview <- buildPreviewOf config path decoder >>= shouldBeOnlyPreviewOf config path
        isRedImage preview `shouldReturn` True
        -- Once built, the preview is reused, not built again.
        buildPreviewOf config path decoder >>= \case
          Right (built, _, cached) -> (built, cached) `shouldBe` (False, preview)
          Left e -> expectationFailure ("Reusing the preview failed: " ++ show e)
        listDirectory (takeDirectory preview) `shouldReturn` [takeFileName preview]
    it "builds the preview of an animated WebP with many frames" $ \config -> do
      requireConvert
      -- ImageMagick decodes all the frames of an animated WebP, even
      -- with [0], so this fails if the policy limits how many images
      -- it reads from a file (list-length) to fewer than there are.
      -- The frames all differ, so that none is merged into another.
      let path = cfgCacheDir config </> "animation"
          frame :: Int -> [String]
          frame i = ["-size", "32x32", "xc:rgb(" ++ show (i * 8) ++ ",0,255)"]
      convertTestImage (["-size", "32x32", "xc:red"] ++ concatMap frame [1 .. 29] ++
                        ["webp:" ++ path])
      -- Each frame prints the number of frames.
      frames <- readMay . takeWhile (/= '\n') <$> imageProperty path "%n\n"
      frames `shouldBe` Just (30 :: Int)
      preview <- buildPreviewOf config path "webp" >>= shouldBeOnlyPreviewOf config path
      isRedImage preview `shouldReturn` True
    it "pads a thumbnail to a square, with transparency" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "wide.png"
          thumb = scaledImagePath config path (cfgThumbnailSize config)
          size = show (cfgThumbnailSize config)
      convertTestImage ["-size", "256x128", "xc:red", "png:" ++ path]
      result <- buildThumbnail config path (Right "png")
      void $ shouldBeOnlyPreview result thumb
      imageProperty thumb "%wx%h" `shouldReturn` (size ++ "x" ++ size)
      isRedImage thumb `shouldReturn` True
      -- Above the (scaled down) picture, in the middle.
      imageProperty thumb "%[fx:p{w/2,h/8}.a<0.2]" `shouldReturn` "1"
    forM_ ([((4000, 100), (1920, 48)), ((64, 32), (64, 32))] :: [((Int, Int), (Int, Int))]) $
      \((w, h), expected) ->
      it ("scales a " ++ show w ++ "x" ++ show h ++ " picture down, but never up") $ \config -> do
        requireConvert
        let path = cfgCacheDir config </> "picture.png"
            showSize (a, b) = show a ++ "x" ++ show b
        convertTestImage ["-size", showSize (w, h), "xc:red", "png:" ++ path]
        preview <- buildPreviewOf config path "png" >>= shouldBeOnlyPreviewOf config path
        imageProperty preview "%wx%h" `shouldReturn` showSize expected
    it "builds a single thumbnail for a file with several images" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "multi.tif"
          thumb = scaledImagePath config path (cfgThumbnailSize config)
      convertTestImage ["-size", "64x64", "xc:red", "xc:blue", "tiff:" ++ path]
      result <- buildThumbnail config path (Right "tiff")
      -- Without the [0], convert would try to write each image to a
      -- numbered file (/dev/fd/1-0, …), and fail.
      void $ shouldBeOnlyPreview result thumb
      isRedImage thumb `shouldReturn` True
    it "doesn't record the path of the picture in its thumbnail" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "photo.jpg"
          thumb = scaledImagePath config path (cfgThumbnailSize config)
      writePreviewJpeg path
      result <- buildThumbnail config path (Right "jpeg")
      contents <- toStrict <$> shouldBeOnlyPreview result thumb
      -- -thumbnail records the name of its input in the Thumb::URI
      -- property, a PNG text chunk, compressed when long (so that the
      -- path wouldn't be found as such); its key never is.
      contents `shouldSatisfy` not . ("Thumb::URI" `isInfixOf`)
      contents `shouldSatisfy` not . (encodeUtf8 (pack (cfgCacheDir config)) `isInfixOf`)
    it "reads the given file, and names the preview after the picture" $ \config -> do
      requireConvert
      -- As for a raw file, whose preview is built from its extracted
      -- embedded image.
      let picture = cfgCacheDir config </> "raw" </> "photo.nef"
          extracted = cfgCacheDir config </> "extracted" </> "embedded.jpg"
      createDirectoryIfMissing True (takeDirectory extracted)
      writePreviewJpeg extracted
      tools <- testToolLimits
      result <- try $ loadCachedOrBuild tools config picture extracted (Right "jpeg")
                        "image/jpeg" 0 previewSize
      void $ shouldBeOnlyPreviewOf config picture result
    it "fails if the picture can't be read, naming it" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "missing.jpg"
      result <- buildThumbnail config path (Right "jpeg")
      shouldFailToBuild config path result
      case result of
        Left (ImageError msg) -> msg `shouldSatisfy` (pack path `isInfixOf`)
        _ -> expectationFailure ("Unexpected result: " ++ show result)
    it "reports convert failing to decode the picture" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "photo.jpg"
      writeFile path ("not a JPEG image" :: ByteString)
      result <- buildThumbnail config path (Right "jpeg")
      shouldFailToBuild config path result
      case result of
        Left (ImageError msg) -> msg `shouldSatisfy` ("convert" `isInfixOf`)
        _ -> expectationFailure ("Unexpected result: " ++ show result)
    it "fails with an image error if the preview can't be written" $ \config -> do
      requireConvert
      -- A valid name, but not with the suffix of the cache file. Any
      -- other exception would escape 'buildPreview', failing the test.
      let path = cfgCacheDir config </> take 248 tooLongName ++ ".jpg"
      writePreviewJpegAs path
      buildPreview config path >>= \case
        -- The error names where the preview was to be written.
        Left (ImageError msg) -> msg `shouldSatisfy` (pack (cfgCacheDir config) `isInfixOf`)
        result -> expectationFailure ("Unexpected result: " ++ show result)
    -- convert expands format specifiers in file names (with an image
    -- selected, in input names too), whether of the file or of a
    -- folder; as it gets no path, none applies. The others are guards:
    -- convert also gives brackets and other percent signs a meaning.
    forM_ ["100%d.jpg", "d%d/photo.jpg", "photo[1].jpg", "50%.jpg"] $ \name ->
      it ("builds the preview of a file named " ++ name) $ \config -> do
        requireConvert
        let path = cfgCacheDir config </> name
        writePreviewJpegAs path
        buildPreview config path >>= void . shouldBeOnlyPreviewOf config path
  describe "ImageMagick policy" $ withConfig $ do
    it "is used by the test configuration" $ \config ->
      cfgMagickPolicyDir config `shouldBe` Just "config/magick"
    it "is loaded by ImageMagick" $ \config -> do
      requireConvert
      checkMagickPolicy config `shouldReturn` Right ()
    it "sets the resource limits" $ \config -> do
      requireConvert
      pconfig <- convertProcess config ["-list", "resource"]
      result <- readToolProcess pconfig
      case result of
        Right (ExitSuccess, out, _) -> do
          let limits = decodeToolOutput out
          forM_ ["Area: 256MP", "Memory: 1GiB", "Map: 2GiB", "Disk: 2GiB"] $ \limit ->
            limits `shouldSatisfy` (limit `isInfixOf`)
          -- ImageMagick versions show these as 32KP or 32000P.
          forM_ ["Width", "Height"] $ \name ->
            limits `shouldSatisfy` \l -> any (`isInfixOf` l)
              [name <> ": 32KP", name <> ": 32000P"]
        _ -> expectationFailure ("Unexpected result: " ++ show result)
    it "does not need checking if not configured" $ \config ->
      checkMagickPolicy config { cfgMagickPolicyDir = Nothing } `shouldReturn` Right ()
    it "fails the check if the policy file is missing" $ \config -> do
      let config' = config { cfgMagickPolicyDir = Just (cfgCacheDir config) }
      checkMagickPolicy config' >>= (`shouldBeLeftWithMessage` "not found")
    it "allows reading with all the default decoders" $ \_ -> do
      readable <- map toUpper <$> policyReadableCoders
      forM_ (setFromList (toList defaultImageDecoders) :: Set Text) $ \decoder ->
        readable `shouldContain` [toUpper decoder]
    it "refuses other decoders, even if passed explicitly" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "document.ps"
      writeFile path postScriptDocument
      buildThumbnail config path (Right "ps") >>= shouldFailToBuild config path
    it "allows writing JPEG previews" $ \config -> do
      requireConvert
      let path = cfgCacheDir config </> "photo.jpg"
      writePreviewJpeg path
      result <- buildPreview config path
      expected <- previewPath config path
      case result of
        Right r -> r `shouldBe` (True, "image/jpeg", expected)
        Left e  -> expectationFailure ("Building the preview failed: " ++ show e)
      contents <- readFile expected
      jpegMagic `BSL.isPrefixOf` fromStrict contents `shouldBe` True
    it "leaves no preview behind if writing it is refused" $ \config -> do
      requireConvert
      -- A policy that allows reading JPEG files, but not writing them.
      let policyDir = cfgCacheDir config </> "read-only-policy"
          config' = config { cfgMagickPolicyDir = Just policyDir }
          path = cfgCacheDir config </> "photo.jpg"
      createDirectoryIfMissing True policyDir
      writeFile (policyDir </> "policy.xml")
        ("<policymap>\n\
         \  <policy domain=\"coder\" rights=\"none\" pattern=\"*\"/>\n\
         \  <policy domain=\"coder\" rights=\"read\" pattern=\"{JPEG,JPG}\"/>\n\
         \</policymap>\n" :: ByteString)
      writePreviewJpeg path
      result <- buildPreview config' path
      result `shouldSatisfy` isLeft
      expected <- previewPath config path
      doesFileExist expected `shouldReturn` False
      -- Nor any temporary file.
      listDirectory (takeDirectory expected) `shouldReturn` []
  describe "embedded images" $ do
    it "passes the path to exiftool after the options" $ do
      embeddedArgs "PreviewImage" "-x.nef" `shouldBe`
        ["-b", "-PreviewImage", "--", "-x.nef"]
    let largeEnough = BSL.replicate 1024 0
        -- Checks an image extracted as a whole.
        check code image = checkEmbeddedImage "PreviewImage" code
                             (BSL.length image) (BSL.take 3 image)
    it "starts all JPEG images with the JPEG magic" $
      withConfig' $ \config -> do
        requireConvert
        let path = cfgCacheDir config </> "real.jpg"
        writeRealJpeg path
        contents <- readFile path
        jpegMagic `BSL.isPrefixOf` fromStrict contents `shouldBe` True
    it "accepts JPEG images" $ \_ -> do
      check ExitSuccess (jpegMagic <> largeEnough) "" `shouldBe` Right ()
    it "refuses images which are not JPEGs" $ \_ ->
      check ExitSuccess ("\x89PNG\r\n" <> largeEnough) ""
        `shouldBeLeftWithMessage` "PreviewImage is not a JPEG"
    it "refuses too small images" $ \_ ->
      check ExitSuccess jpegMagic ""
        `shouldBeLeftWithMessage` "PreviewImage is too small (3 bytes)"
    it "reports a missing embedded image" $ \_ ->
      check ExitSuccess "" ""
        `shouldBeLeftWithMessage` "No embedded image PreviewImage found"
    it "returns the error of a failed extraction" $ \_ -> do
      let result = check (ExitFailure 1) (jpegMagic <> largeEnough) "tool error"
      result `shouldBeLeftWithMessage` "PreviewImage"
      result `shouldBeLeftWithMessage` "tool error"
    it "extracts an embedded JPEG image" $
      withConfig' $ \config -> do
        requireExifTool
        requireConvert
        let preview = cfgCacheDir config </> "preview.jpg"
            path = cfgCacheDir config </> "photo.mie"
        writePreviewJpeg preview
        writeFileWithPreview preview path
        result <- extractEmbeddedT config path 0 "PreviewImage"
        case result of
          Left msg -> expectationFailure ("Failed to extract the preview: " ++ show msg)
          Right (mime, extracted) -> do
            mime `shouldBe` "image/jpeg"
            expected <- readFile preview
            readFile extracted `shouldReturn` asByteString expected
    it "returns an error if the image can't be written" $
      withConfig' $ \config -> do
        requireExifTool
        requireConvert
        let preview = cfgCacheDir config </> "preview.jpg"
            path = cfgCacheDir config </> "blocked" </> "photo.mie"
        createDirectoryIfMissing True (takeDirectory path)
        writePreviewJpeg preview
        writeFileWithPreview preview path
        blockOutput (embeddedImagePath config path)
        result <- extractEmbeddedT config path farFutureTime "PreviewImage"
        result `shouldSatisfy` isLeft
    it "returns an error if the image's cache directory is a file" $
      withConfig' $ \config -> do
        requireExifTool
        requireConvert
        let preview = cfgCacheDir config </> "preview.jpg"
            path = cfgCacheDir config </> "photo.mie"
        writePreviewJpeg preview
        writeFileWithPreview preview path
        blockCacheDir config path
        result <- extractEmbeddedT config path 0 "PreviewImage"
        result `shouldSatisfy` isLeft
    it "waits for a slot before creating any file" $
      withConfig' $ \config -> do
        -- What matters is that nothing is created while it waits for
        -- the slot (see 'waitsForSlotFirst'), not its result: the
        -- picture doesn't exist, so it fails once it runs, and needs
        -- neither exiftool nor a real picture.
        let path = cfgCacheDir config </> "photo.mie"
        _ <- waitsForSlotFirst config (embeddedImagePath config path) $ \tools ->
          extractEmbedded tools config path 0 "PreviewImage"
        return ()
    it "fails if there is no embedded image" $
      withConfig' $ \config -> do
        requireExifTool
        requireConvert
        let preview = cfgCacheDir config </> "preview.jpg"
            path = cfgCacheDir config </> "photo.mie"
        writePreviewJpeg preview
        writeFileWithPreview preview path
        result <- extractEmbeddedT config path 0 "JpgFromRaw"
        result `shouldBeLeftWithMessage` "No embedded image JpgFromRaw found"
        -- Nor is any (temporary) file left behind.
        let output = embeddedImagePath config path
        doesFileExist output `shouldReturn` False
        listDirectory (takeDirectory output) `shouldReturn` []
    forM_ [(False, "reuses an up to date", 0), (True, "rebuilds an outdated", farFutureTime)] $
      \(rebuilt, what, mtime) ->
        it (what ++ " embedded image") $
          withConfig' $ \config -> do
            requireExifTool
            requireConvert
            let preview = cfgCacheDir config </> "preview.jpg"
                path = cfgCacheDir config </> "photo.mie"
            writePreviewJpeg preview
            writeFileWithPreview preview path
            expected <- readFile preview
            extractEmbeddedT config path 0 "PreviewImage" >>= \case
              Left msg -> expectationFailure ("Failed to extract the preview: " ++ show msg)
              Right (_, extracted) -> do
                writeFile extracted ("old" :: ByteString)
                _ <- extractEmbeddedT config path mtime "PreviewImage"
                readFile extracted `shouldReturn`
                  (if rebuilt then expected else "old")
  describe "movie frames" $ withConfig $ do
    it "extracts the first frame as a JPEG" $ \config -> do
      requireFfmpeg
      let path = cfgCacheDir config </> "movie.mp4"
      writeMovie path
      result <- extractFirstFrameT config path 0
      case result of
        Left msg -> expectationFailure ("Failed to extract the frame: " ++ show msg)
        Right (mime, frame) -> do
          mime `shouldBe` "image/jpeg"
          contents <- readFile frame
          jpegMagic `BSL.isPrefixOf` fromStrict contents `shouldBe` True
    it "extracts the first frame of an AVI movie" $ \config -> do
      requireFfmpeg
      let path = cfgCacheDir config </> "movie.avi"
      writeMovie path
      extractFirstFrameT config path 0 >>= \case
        Left msg -> expectationFailure ("Failed to extract the frame: " ++ show msg)
        Right (_, frame) -> do
          contents <- readFile frame
          jpegMagic `BSL.isPrefixOf` fromStrict contents `shouldBe` True
    it "refuses a movie whose format doesn't match its extension" $ \config -> do
      requireFfmpeg
      let avi = cfgCacheDir config </> "real.avi"
          path = cfgCacheDir config </> "fake.mp4"
      writeMovie avi
      renameFile avi path
      result <- extractFirstFrameT config path 0
      result `shouldSatisfy` isLeft
      doesFileExist (embeddedImagePath config path) `shouldReturn` False
    it "refuses a concat script named as a movie" $ \config -> do
      requireFfmpeg
      -- ffmpeg itself would accept this script (it only refuses
      -- absolute paths and the like), and read the other movie.
      let dir = cfgCacheDir config
          path = dir </> "script.mp4"
      writeMovie (dir </> "other.mp4")
      writeFile path ("ffconcat version 1.0\nfile 'other.mp4'\n" :: ByteString)
      result <- extractFirstFrameT config path 0
      result `shouldSatisfy` isLeft
      doesFileExist (embeddedImagePath config path) `shouldReturn` False
    it "returns an error if the frame's cache file name is too long" $ \config -> do
      requireFfmpeg
      -- The movie's name is valid, but not with the cache suffix.
      let path = cfgCacheDir config </> take 248 tooLongName ++ ".mp4"
      writeMovie path
      -- The cache directory exists, as once other files in the folder
      -- have cache files; otherwise, checking the cache file only finds
      -- that its directory is missing.
      createDirectoryIfMissing True (takeDirectory (embeddedImagePath config path))
      result <- extractFirstFrameT config path 0
      result `shouldSatisfy` isLeft
    it "returns an error if the frame's cache directory is a file" $ \config -> do
      requireFfmpeg
      let path = cfgCacheDir config </> "movie.mp4"
      writeMovie path
      blockCacheDir config path
      result <- extractFirstFrameT config path 0
      result `shouldSatisfy` isLeft
    it "leaves no file behind on failure" $ \config -> do
      requireFfmpeg
      let dir = cfgCacheDir config </> "sub"
          path = dir </> "broken.mp4"
      createDirectoryIfMissing True dir
      writeFile path ("not a movie" :: ByteString)
      extractFirstFrameT config path 0 >>= (`shouldSatisfy` isLeft)
      let (outDir, _) = splitFileName (embeddedImagePath config path)
      outputs <- listDirectory outDir
      outputs `shouldSatisfy` all (\f -> not ("broken.mp4-" `isPrefixOf` f))
      listDirectory (cacheTempDir config) `shouldReturn` []
    it "reports only ffmpeg's errors" $ \config -> do
      requireFfmpeg
      let path = cfgCacheDir config </> "broken.mp4"
      writeFile path ("not a movie" :: ByteString)
      result <- extractFirstFrameT config path 0
      case result of
        -- Without its banner (which starts with "ffmpeg version").
        Left msg -> do
          msg `shouldSatisfy` (not . null)
          msg `shouldSatisfy` (not . ("ffmpeg version" `isInfixOf`))
        Right _ -> expectationFailure "A broken movie was accepted"
    it "asks ffmpeg for errors only, reading the movie as a local file" $ \_ -> do
      let args = firstFrameArgs "mov" "/p/a:b.mov" "/c/out"
      args `shouldSatisfy` (["-loglevel", "error"] `isInfixOf`)
      args `shouldSatisfy` (["-f", "mov", "-i", "file:/p/a:b.mov"] `isInfixOf`)
      lastMay args `shouldBe` Just "/c/out"
    it "returns an error if the frame can't be written" $ \config -> do
      requireFfmpeg
      let path = cfgCacheDir config </> "blocked" </> "movie.mp4"
      createDirectoryIfMissing True (takeDirectory path)
      writeMovie path
      blockOutput (embeddedImagePath config path)
      result <- extractFirstFrameT config path farFutureTime
      result `shouldSatisfy` isLeft
    it "waits for a slot before creating any file" $ \config -> do
      -- What matters is that nothing is created while it waits for the
      -- slot (see 'waitsForSlotFirst'), not its result: the movie
      -- doesn't exist, so it fails once it runs, and needs neither
      -- ffmpeg nor a real movie.
      let path = cfgCacheDir config </> "movie.mp4"
      _ <- waitsForSlotFirst config (embeddedImagePath config path) $ \tools ->
        extractFirstFrame tools config path 0
      return ()
    it "refuses movies without a configured demuxer" $ \config -> do
      let path = cfgCacheDir config </> "movie.xyz"
      writeFile path ("whatever" :: ByteString)
      result <- extractFirstFrameT config path 0
      result `shouldBeLeftWithMessage` "No movie demuxer configured for extension 'xyz'"
    forM_ [(False, "reuses an up to date", 0), (True, "rebuilds an outdated", farFutureTime)] $
      \(rebuilt, what, mtime) ->
        it (what ++ " frame") $ \config -> do
          requireFfmpeg
          let path = cfgCacheDir config </> "movie.mp4"
          writeMovie path
          extractFirstFrameT config path 0 >>= \case
            Left msg -> expectationFailure ("Failed to extract the frame: " ++ show msg)
            Right (_, frame) -> do
              writeFile frame ("old" :: ByteString)
              _ <- extractFirstFrameT config path mtime
              contents <- readFile frame
              jpegMagic `BSL.isPrefixOf` fromStrict contents `shouldBe` rebuilt
  describe "bestMovie" $ withConfig $ do
    let movie = simpleFile "a.mp4"
        master = simpleFile "a.mov"
        withMovies config movs mmaster =
          (mkImage config "a" "folder" Nothing Nothing [simpleFile "a.jpg"]
             Nothing [] [] Nothing MediaImage def)
            { imgMovs = movs, imgMasterMov = mmaster }
    it "prefers the processed movies" $ \config ->
      bestMovie (withMovies config [movie] (Just master)) `shouldBe` Just movie
    it "falls back to the master movie" $ \config ->
      bestMovie (withMovies config [] (Just master)) `shouldBe` Just master
    it "returns nothing for images without movies" $ \config ->
      bestMovie (withMovies config [] Nothing) `shouldBe` Nothing
  describe "build file from inode" $ do
    it "builds a file from an inode with no subdirs" $ \_ -> do
      let ii = InodeInfo "file.jpg" [] False 0 0 0
          exif = def
          f = mkFileFromInode (mkSym "/pics/2022") ii exif
      fileName f `shouldBe` "file.jpg"
      fileParent f `shouldBe` mkSym "/pics/2022"
      fileDirs f `shouldBe` mkSym ""
      fileCTime f `shouldBe` 0
      fileMTime f `shouldBe` 0
      fileSize f `shouldBe` 0
      fileExif f `shouldBe` exif
      fileFullPath f `shouldBe` "/pics/2022/file.jpg"
      fileRelPath f `shouldBe` "file.jpg"
    it "builds a file from an inode" $ \_ -> do
      let ii = InodeInfo "file.jpg" ["subdir", "dir"] False 0 0 0
          exif = def
          f = mkFileFromInode (mkSym "/pics/2022") ii exif
      fileName f `shouldBe` "file.jpg"
      fileParent f `shouldBe` mkSym "/pics/2022"
      fileDirs f `shouldBe` mkSym "dir/subdir"
      fileCTime f `shouldBe` 0
      fileMTime f `shouldBe` 0
      fileSize f `shouldBe` 0
      fileExif f `shouldBe` exif
      fileFullPath f `shouldBe` "/pics/2022/dir/subdir/file.jpg"
      fileRelPath f `shouldBe` "dir/subdir/file.jpg"
  describe "computeRepoStats" $ do
    it "returns empty event stats for an empty repository" $ \_ -> do
      rsEventStats (computeRepoStats Map.empty) `shouldBe` Map.empty
    it "counts folders by event kind" $ \_ -> do
      let noEvent = createTestPicDir "none"
          birthday = (createTestPicDir "bday")
           { pdEvent = Just BirthdayEvent { eventName = "x", eventPeople = [], eventSource = EventExplicit Nothing } }
          generic1 = (createTestPicDir "g1")
           { pdEvent = Just GenericEvent { eventName = "a", eventPeople = [], eventSource = EventExplicit Nothing } }
          generic2 = (createTestPicDir "g2")
           { pdEvent = Just GenericEvent { eventName = "b", eventPeople = [], eventSource = EventExplicit Nothing } }
          dirs = Map.fromList [(pdName d, d) | d <- [noEvent, birthday, generic1, generic2]]
      rsEventStats (computeRepoStats dirs) `shouldBe` Map.fromList
        [ (EKNoEvent, 1)
        , (EKBirthday, 1)
        , (EKGeneric, 2)
        ]
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
  describe "implicitEventFromDateRange" $ do
    let day d = LocalTime (fromGregorian 2024 6 d) midnight
        range a b = Just (day a, day b)
        name = "folder" :: ShortText
        kindOf = extractEventType . implicitEventFromDateRange name
    it "does nothing without a date range" $ \_ ->
      kindOf Nothing `shouldBe` EKNoEvent
    it "does nothing for a same-day span" $ \_ ->
      kindOf (range 1 1) `shouldBe` EKNoEvent
    it "does nothing for a two-day span" $ \_ ->
      kindOf (range 1 3) `shouldBe` EKNoEvent
    it "uses getaway for a three-day span" $ \_ ->
      kindOf (range 1 4) `shouldBe` EKGetaway
    it "uses getaway for a six-day span" $ \_ ->
      kindOf (range 1 7) `shouldBe` EKGetaway
    it "uses grand vacation for a seven-day span" $ \_ ->
      kindOf (range 1 8) `shouldBe` EKGrandVacation
    it "uses grand vacation for a longer span" $ \_ ->
      kindOf (range 1 15) `shouldBe` EKGrandVacation
    it "marks inferred events as implicit with the folder name" $ \_ ->
      implicitEventFromDateRange name (range 1 4) `shouldBe`
        Just GetawayEvent { eventName = name, eventPeople = [], eventSource = EventImplicit implicitDateRangeDesc }
  describe "makeRel" $ do
    it "strips leading separators" $
      makeRel "/foo/bar" `shouldBe` "foo/bar"
    it "leaves relative paths unchanged" $
      makeRel "foo/bar" `shouldBe` "foo/bar"
  describe "findBestSize" $ do
    let sizes = Set.fromList [64, 1024, 1920]
    it "returns the greatest size not above the request" $
      findBestSize (ImageSize 1024) sizes `shouldBe` Just 1024
    it "steps down when the request is between sizes" $
      findBestSize (ImageSize 1000) sizes `shouldBe` Just 64
    it "returns Nothing when the request is below all sizes" $
      findBestSize (ImageSize 32) sizes `shouldBe` Nothing
  withConfig $ do
    describe "scaled cache freshness" $ do
      it "treats a missing cache file as needing a rebuild" $ \config ->
        cacheFileNeedsBuild (cfgCacheDir config </> "missing") 0 `shouldReturn` True
      it "treats an older cache file as needing a rebuild" $ \config -> do
        let path = cfgCacheDir config </> "stale"
        touchFile path
        cacheFileNeedsBuild path farFutureTime `shouldReturn` True
      it "treats a newer cache file as up to date" $ \config -> do
        let path = cfgCacheDir config </> "fresh"
        writeNonEmpty path
        cacheFileNeedsBuild path 0 `shouldReturn` False
      it "treats a cache file in a non-directory as needing a rebuild" $ \config -> do
        let parent = cfgCacheDir config </> "file"
        writeNonEmpty parent
        cacheFileNeedsBuild (parent </> "preview") 0 `shouldReturn` True
      it "treats a cache file with a too long name as needing a rebuild" $ \config ->
        cacheFileNeedsBuild (cfgCacheDir config </> tooLongName) 0 `shouldReturn` True
      it "treats an empty cache file as needing a rebuild" $ \config -> do
        -- E.g. as left behind by a failed conversion in older versions.
        let path = cfgCacheDir config </> "empty"
        touchFile path
        cacheFileNeedsBuild path 0 `shouldReturn` True
      it "does not need a scaled cache when no configured size applies" $ \config ->
        -- Note: this works as long as size 1 is not in the test image sizes.
        scaledCacheNeedsBuild config "x.jpg" 0 (ImageSize 1) `shouldReturn` False
      it "needs a rebuild when the scaled cache is missing" $ \config ->
        -- Note: this again works if the 1920 or smaller is in the test sizes.
        scaledCacheNeedsBuild config (cfgCacheDir config </> "src.jpg") 0 (ImageSize 1920)
          `shouldReturn` True
      it "is up to date when the scaled cache is newer than the source" $ \config -> do
        -- Note: again, the size needs to be in sync with the image sizes
        -- set for the test config.
        let orig = cfgCacheDir config </> "src.jpg"
            size = ImageSize 1920
        case findBestSize size (cfgAllImageSizes config) of
          Nothing -> expectationFailure "expected a cached size for 1920"
          Just res -> do
            writeNonEmpty (scaledImagePath config orig res)
            scaledCacheNeedsBuild config orig 0 size `shouldReturn` False
    describe "writeViaTempFile" $ do
      let target config = cfgCacheDir config </> "sub" </> "file"
          entries = listDirectory . takeDirectory
      it "creates the file, and its directory" $ \config -> do
        writeViaTempFile config (target config) $ \tmp ->
          writeFile tmp ("contents" :: ByteString)
        readFile (target config) `shouldReturn` ("contents" :: ByteString)
        entries (target config) `shouldReturn` ["file"]
        listDirectory (cacheTempDir config) `shouldReturn` []
      it "writes to another file, in the cache's temporary directory" $ \config -> do
        writeViaTempFile config (target config) $ \tmp -> do
          takeDirectory tmp `shouldBe` cacheTempDir config
          doesFileExist (target config) `shouldReturn` False
          writeFile tmp ("x" :: ByteString)
      it "replaces an existing file" $ \config -> do
        writeNonEmpty (target config)
        writeViaTempFile config (target config) $ \tmp ->
          writeFile tmp ("new" :: ByteString)
        readFile (target config) `shouldReturn` ("new" :: ByteString)
      it "leaves nothing behind on failure, and rethrows" $ \config -> do
        let failing tmp = do
              writeFile tmp ("partial" :: ByteString)
              throwIO (ImageError "conversion failed")
        writeViaTempFile config (target config) failing
          `shouldThrow` (\case ImageError msg -> msg == "conversion failed"
                               _ -> False)
        entries (target config) `shouldReturn` []
        listDirectory (cacheTempDir config) `shouldReturn` []
      it "writes a file whose name is near the length limit" $ \config -> do
        -- 251 bytes: valid, but too long once the temporary file's
        -- random part is added to it.
        let long = cfgCacheDir config </> "sub" </> (replicate 245 'a' ++ ".jpg-64")
        writeViaTempFile config long $ \tmp ->
          writeFile tmp ("contents" :: ByteString)
        readFile long `shouldReturn` ("contents" :: ByteString)
      it "keeps files of the same name, written at once, apart" $ \config -> do
        -- The temporary files of all the directories are in the same
        -- one, so files of the same name must still get their own.
        let one = cfgCacheDir config </> "x" </> "a.jpg-64"
            two = cfgCacheDir config </> "y" </> "a.jpg-64"
        writeViaTempFile config one $ \tmp1 -> do
          writeFile tmp1 ("first" :: ByteString)
          writeViaTempFile config two $ \tmp2 -> do
            tmp2 `shouldNotBe` tmp1
            writeFile tmp2 ("second" :: ByteString)
        readFile one `shouldReturn` ("first" :: ByteString)
        readFile two `shouldReturn` ("second" :: ByteString)
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
  withConfig $ do
    describe "NFData" $ do
      it "forces the media type" $ \_ ->
        evaluate (rnf (error "type" :: MediaType)) `shouldThrow` anyErrorCall
      it "forces image movie and untracked files" $ \config -> do
        let img = simpleRawImage config
        evaluate (rnf img) `shouldReturn` ()
        evaluate (rnf (img { imgMasterMov = Just (error "master-mov") }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (img { imgMovs = [error "mov"] }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (img { imgUntracked = [error "untracked"] }))
          `shouldThrow` anyErrorCall
      it "forces promoted image exif" $ \config -> do
        let img = simpleRawImage config
            thunked = img { imgExif = def { exifTitle = Just (error "title") } }
        evaluate (rnf thunked) `shouldThrow` anyErrorCall
      it "forces folder time sort, stats date range, and events" $ \_ -> do
        let dir = createTestPicDir "test"
        evaluate (rnf dir) `shouldReturn` ()
        evaluate (rnf (dir { pdTimeSort = Set.singleton (Just (error "time"), "a") }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (dir { pdEvent = Just (error "event") }))
          `shouldThrow` anyErrorCall
        evaluate (rnf (dir { pdStats = def { sDateRange = Just (error "range") } }))
          `shouldThrow` anyErrorCall
    describe "splitPathExt and isKnownMediaInode" $ do
      it "recognises configured media extensions" $ \config -> do
        isKnownMediaInode config (mkInode "a.nef") `shouldBe` True
        isKnownMediaInode config (mkInode "a.jpg") `shouldBe` True
        isKnownMediaInode config (mkInode "a.xmp") `shouldBe` True
        isKnownMediaInode config (mkInode "a.mov") `shouldBe` True
      it "rejects unknown extensions and extensionless names" $ \config -> do
        isKnownMediaInode config (mkInode "notes.other") `shouldBe` False
        isKnownMediaInode config (mkInode "SHA1SUMS") `shouldBe` False
        isKnownMediaInode config (mkInode "corydalis.yaml") `shouldBe` False
        splitPathExt "SHA1SUMS" `shouldBe` ("SHA1SUMS", "")
        splitPathExt "a.nef" `shouldBe` ("a", "nef")
        splitPathExt "sub/a.nef" `shouldBe` ("sub/a", "nef")
    describe "path regex helpers" $ do
      it "matches date-prefixed folder names" $ \config -> do
        isOKDir config "2024-01-01-trip" `shouldBe` True
        isOKDir config "not-a-date" `shouldBe` False
        isOKDir config "level1" `shouldBe` False
      it "drops copy suffixes" $ \config -> do
        dropCopySuffix config "IMG_1234-1" `shouldBe` "IMG_1234"
        dropCopySuffix config "IMG_1234-Edit" `shouldBe` "IMG_1234"
        dropCopySuffix config "IMG_1234" `shouldBe` "IMG_1234"
      it "drops copy suffixes only from the file name" $ \config' -> do
        let config = withUnanchoredNameRegexes config'
        dropCopySuffix config "sub/IMG_1234-1" `shouldBe` "sub/IMG_1234"
        dropCopySuffix config "2024-01-02-day/b" `shouldBe` "2024-01-02-day/b"
        dropCopySuffix config "a-1/b-Edit" `shouldBe` "a-1/b"
      it "drops copy suffixes only at the end of the name" $ \config ->
        dropCopySuffix config "IMG-20240101_1234" `shouldBe` "IMG-20240101_1234"
      it "expands ranges only from the file name" $ \config' -> do
        let config = withUnanchoredNameRegexes config'
        expandRangeFile config "sub/img_1-2" `shouldBe` ["sub/img_1", "sub/img_2"]
        expandRangeFile config "a/b/img_1-2" `shouldBe` ["a/b/img_1", "a/b/img_2"]
        expandRangeFile config "img_1-2/b" `shouldBe` []
      it "expands padded range names" $ \config -> do
        expandRangeFile config "img_01-03" `shouldBe` ["img_01", "img_02", "img_03"]
        expandRangeFile config "img_1-3" `shouldBe` ["img_1", "img_2", "img_3"]
        expandRangeFile config "nope" `shouldBe` []
      it "rejects inverted or huge ranges" $ \config -> do
        expandRangeFile config "img_9-1" `shouldBe` []
        expandRangeFile config "img_1-1025" `shouldBe` []
        expandRangeFile config "img_3982-1773958749527" `shouldBe` []
        length (expandRangeFile config "img_1-1024") `shouldBe` maxRangeExpansion
    describe "mkImageStatus" $ do
      it "classifies backing-file combinations" $ \config -> do
        let raw = simpleFile "a.nef"
            jpeg = simpleFile "a.jpg"
            sidecar = simpleFile "a.xmp"
        mkImageStatus config (Just raw) [] Nothing False `shouldBe` ImageUnprocessed
        mkImageStatus config (Just raw) [jpeg] Nothing False `shouldBe` ImageProcessed
        mkImageStatus config Nothing [jpeg] Nothing False `shouldBe` ImageStandalone
        mkImageStatus config Nothing [] (Just sidecar) False `shouldBe` ImageOrphaned
        mkImageStatus config Nothing [jpeg] (Just sidecar) False `shouldBe` ImageStandalone
        mkImageStatus config Nothing [] Nothing True `shouldBe` ImageProcessed
      it "errors when nothing backs the image" $ \config ->
        evaluate (mkImageStatus config Nothing [] Nothing False) `shouldThrow` anyErrorCall
    describe "loadImage" $ do
      it "treats a source raw as unprocessed" $ \config -> do
        let (img, shadows) = runLoad config True (mkInode "a.nef")
        imgName img `shouldBe` "a"
        imgStatus img `shouldBe` ImageUnprocessed
        imgType img `shouldBe` MediaImage
        isJust (imgRawPath img) `shouldBe` True
        shadows `shouldBe` []
      it "treats an output jpeg as standalone" $ \config -> do
        let (img, _) = runLoad config False (mkInode "a.jpg")
        imgStatus img `shouldBe` ImageStandalone
        null (imgJpegPath img) `shouldBe` False
        isNothing (imgRawPath img) `shouldBe` True
      it "treats a source jpeg as a soft master" $ \config -> do
        let (img, _) = runLoad config True (mkInode "a.jpg")
        flagsSoftMaster (imgFlags img) `shouldBe` True
        isJust (imgRawPath img) `shouldBe` True
        imgJpegPath img `shouldBe` []
        imgStatus img `shouldBe` ImageUnprocessed
      it "treats a sidecar-only file as orphaned without shadows" $ \config -> do
        let (img, shadows) = runLoad config False (mkInode "a.xmp")
        imgStatus img `shouldBe` ImageOrphaned
        isJust (imgSidecarPath img) `shouldBe` True
        shadows `shouldBe` []
      it "classifies source movies as master movies" $ \config -> do
        let (img, _) = runLoad config True (mkInode "clip.mov")
        imgType img `shouldBe` MediaMovie
        isJust (imgMasterMov img) `shouldBe` True
        imgMovs img `shouldBe` []
      it "classifies output movies as processed movies" $ \config -> do
        let (img, _) = runLoad config False (mkInode "clip.mp4")
        imgType img `shouldBe` MediaMovie
        imgMasterMov img `shouldBe` Nothing
        null (imgMovs img) `shouldBe` False
      it "classifies unknown extensions as untracked" $ \config -> do
        let (img, _) = runLoad config True (mkInode "notes.other")
        imgType img `shouldBe` MediaUnknown
        null (imgUntracked img) `shouldBe` False
      it "classifies extensionless files as untracked" $ \config -> do
        let (img, _) = runLoad config True (mkInode "SHA1SUMS")
        imgType img `shouldBe` MediaUnknown
        null (imgUntracked img) `shouldBe` False
      it "expands range jpegs into shadows" $ \config -> do
        let (img, shadows) = runLoad (withProdNameRegexes config) False (mkInode "a_1-3.jpg")
        imgName img `shouldBe` "a_1-3"
        imgRange img `shouldBe` Just ("a_1", "a_3")
        map imgName shadows `shouldBe` ["a_1", "a_2", "a_3"]
      it "drops copy suffixes from the image name" $ \config -> do
        let (img, _) = runLoad config True (mkInode "foo-1.nef")
        imgName img `shouldBe` "foo"
      it "warns when exif is missing from the cache" $ \config -> do
        let (img, _) = runLoad config True (mkInode "a.nef")
        imgProblems img `shouldBe` Set.singleton "exif: Internal error: exif not read"
      it "warns when exif reading failed" $ \config -> do
        let cache = Map.singleton "a.nef" (Left "boom")
            (img, _) = runLoadExif config True (mkInode "a.nef") cache
        imgProblems img `shouldBe` Set.singleton "exif: Cannot read exif: boom"
      it "attaches a successful exif entry" $ \config -> do
        let ex = datedExif 2021 3 4
            cache = Map.singleton "a.nef" (Right ex)
            (img, _) = runLoadExif config True (mkInode "a.nef") cache
        imageYear img `shouldBe` Just 2021
        imageYearMonth img `shouldBe` Just (2021, 3)
      it "classifies nested inodes as raw with relative dirs" $ \config -> do
        let ii = (mkInode "a.nef") { inodeDirs = ["sub"] }
            (img, _) = runLoad config True ii
        imgName img `shouldBe` "sub/a"
        fileRelPath <$> imgRawPath img `shouldBe` Just "sub/a.nef"
    describe "buildFolderFromInodes" $ do
      it "assembles a folder from raw inodes" $ \config -> do
        let folder = buildFolderFromInodes config "2024-01-01" "/pics/2024-01-01" True
                       [mkInode "a.nef"] Map.empty Nothing
        pdName folder `shouldBe` "2024-01-01"
        imgStatus (pdImages folder Map.! "a") `shouldBe` ImageUnprocessed
        pdEvent folder `shouldBe` Nothing
      it "keeps an explicit yaml event" $ \config -> do
        let ev = BirthdayEvent { eventName = "Birthday", eventPeople = [], eventSource = EventExplicit Nothing }
            folder = buildFolderFromInodes config "folder" "/p" True [mkInode "a.nef"] Map.empty (Just ev)
        pdEvent folder `shouldBe` Just ev
      it "infers an implicit event from a multi-day date range" $ \config -> do
        let e1 = datedExif 2024 6 1
            e2 = datedExif 2024 6 5
            cache = Map.fromList [("a.nef", Right e1), ("b.nef", Right e2)]
            folder = buildFolderFromInodes config "folder" "/p" True
                       [mkInode "a.nef", mkInode "b.nef"] cache Nothing
        extractEventType (pdEvent folder) `shouldBe` EKGetaway
      it "dates a folder by its earliest dated image" $ \config -> do
        let cache = Map.fromList [ ("a.nef", Right (datedExif 2024 6 5))
                                 , ("c.nef", Right (datedExif 2024 6 1))
                                 , ("e.nef", Right (datedExif 2024 6 3))
                                 ]
            -- With undated images before, between and after.
            names = ["0.nef", "a.nef", "b.nef", "c.nef", "d.nef", "e.nef", "f.nef"]
            folder = buildFolderFromInodes config "folder" "/p" True
                       (map mkInode names) cache Nothing
        Map.size (pdImages folder) `shouldBe` length names
        pdTimestamp folder `shouldBe` Just (LocalTime (fromGregorian 2024 6 1) midnight)
        pdYear folder `shouldBe` Just 2024
        let undated = buildFolderFromInodes config "folder" "/p" True
                        [mkInode "a.nef"] Map.empty Nothing
        pdTimestamp undated `shouldBe` Nothing
      it "keeps unknown and extensionless files out of the image map" $ \config -> do
        let eImg = datedExif 2024 6 1
            eOther = datedExif 1999 1 1
            cache = Map.fromList
              [ ("a.nef", Right eImg)
              , ("notes.other", Right eOther)
              , ("SHA1SUMS", Right eOther)
              , ("corydalis.yaml", Right eOther)
              ]
            folder = buildFolderFromInodes config "folder" "/p" True
                       [ mkInode "a.nef"
                       , mkInode "notes.other"
                       , mkInode "SHA1SUMS"
                       , mkInode "corydalis.yaml"
                       ] cache Nothing
        Map.keys (pdImages folder) `shouldBe` ["a"]
        map fileName (pdUntracked folder) `shouldMatchList`
          ["notes.other", "SHA1SUMS", "corydalis.yaml"]
        let captured = LocalTime (fromGregorian 2024 6 1) midnight
        pdTimestamp folder `shouldBe` Just captured
        sDateRange (pdStats folder) `shouldBe` Just (captured, captured)
        sUntracked (pdStats folder) `shouldBe` 3
        Map.notMember "notes" (pdImages folder) `shouldBe` True
        Map.notMember "SHA1SUMS" (pdImages folder) `shouldBe` True
        Map.notMember "corydalis" (pdImages folder) `shouldBe` True
      it "does not attach unknown same-basename files to images" $ \config -> do
        let folder = buildFolderFromInodes config "folder" "/p" True
                       [mkInode "a.nef", mkInode "a.txt"] Map.empty Nothing
        imgUntracked (pdImages folder Map.! "a") `shouldBe` []
        map fileName (pdUntracked folder) `shouldBe` ["a.txt"]
    describe "isBetterMaster" $ do
      it "prefers earlier extensions" $ \_ -> do
        isBetterMaster ["nef", "raf"] "nef" "raf" `shouldBe` True
        isBetterMaster ["nef", "raf"] "raf" "nef" `shouldBe` False
      it "chooses the first file when neither extension matches" $ \_ ->
        isBetterMaster ["nef"] "xxx" "yyy" `shouldBe` True
      it "chooses the first file on an empty extension list" $ \_ ->
        isBetterMaster [] "a" "b" `shouldBe` True
    describe "selectMasterFile and mergePictures" $ do
      it "keeps a hard master over a soft one" $ \config -> do
        let hard = simpleRawImage config
            jpeg = simpleFile "a.jpg"
            soft = mkImage config "a" "test" (Just jpeg) Nothing [] Nothing [] [] Nothing MediaImage (Flags True)
            (chosen, softFlag, extra) = selectMasterFile (cfgRawExts config) imgRawPath hard soft
        chosen `shouldBe` imgRawPath hard
        softFlag `shouldBe` False
        extra `shouldBe` [jpeg]
      it "merges raw and jpeg into a processed image" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            merged = mergePictures config raw jpegImg
        imgStatus merged `shouldBe` ImageProcessed
        isJust (imgRawPath merged) `shouldBe` True
        null (imgJpegPath merged) `shouldBe` False
      it "selects nef over raf as the real master" $ \config -> do
        let nef = mkImage config "a" "test" (Just $ simpleFile "a.nef") Nothing [] Nothing [] [] Nothing MediaImage def
            raf = mkImage config "a" "test" (Just $ simpleFile "a.raf") Nothing [] Nothing [] [] Nothing MediaImage def
            merged = mergePictures config nef raf
        fileName <$> imgRawPath merged `shouldBe` Just "a.nef"
        map fileName (imgJpegPath merged) `shouldBe` ["a.raf"]
    describe "mergeFolders" $ do
      it "keeps the path with more raw files as main" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "b" "trip" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def
            d1 = (picDirWith "trip" [raw]) { pdMainPath = "/raw/trip" }
            d2 = (picDirWith "trip" [jpegImg]) { pdMainPath = "/jpg/trip" }
            merged = mergeFolders config d1 d2
        pdMainPath merged `shouldBe` "/raw/trip"
        pdSecPaths merged `shouldContain` ["/jpg/trip"]
      it "recomputes stats after raw and jpeg of the same image merge" $ \config -> do
        let raw = simpleRawImage config
            jpegImg = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            merged = mergeFolders config (picDirWith "test" [raw]) (picDirWith "test" [jpegImg])
        imgStatus (pdImages merged Map.! "a") `shouldBe` ImageProcessed
        sProcessed (pdStats merged) `shouldBe` 1
        sRaw (pdStats merged) `shouldBe` 0
        sStandalone (pdStats merged) `shouldBe` 0
      it "concatenates untracked files and counts them in stats" $ \config -> do
        let d1 = (picDirWith "test" [simpleRawImage config])
                   { pdUntracked = [fileSized "SHA1SUMS" 10] }
            d2 = (createTestPicDir "test")
                   { pdUntracked = [fileSized "notes.other" 7] }
            merged = mergeFolders config d1 d2
        map fileName (pdUntracked merged) `shouldMatchList` ["SHA1SUMS", "notes.other"]
        sUntracked (pdStats merged) `shouldBe` 2
        sUntrackedSize (pdStats merged) `shouldBe` 17
        Map.member "a" (pdImages merged) `shouldBe` True
    describe "mergeShadows and ranges" $ do
      it "applies shadows onto matching images" $ \config -> do
        let raw = simpleRawImage config
            shadow = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            dir = (createTestPicDir "test")
                    { pdImages = Map.singleton "a" raw
                    , pdShadows = Map.singleton "a" shadow
                    }
            merged = mergeShadows config dir
        imgStatus (pdImages merged Map.! "a") `shouldBe` ImageProcessed
      it "promotes a standalone range jpeg when the begin image has a raw" $ \config -> do
        let root = simpleRawImage config
            ranged = mkImage config "a_1" "test" Nothing Nothing [simpleFile "a_1.jpg"]
                             Nothing [] [] (Just ("a", "a_3")) MediaImage def
            dir = picDirWith "test" [root, ranged]
        imgStatus ranged `shouldBe` ImageStandalone
        case maybeUpdateStandaloneRange config dir ranged of
          Nothing -> expectationFailure "expected standalone range to be updated"
          Just img -> do
            imgStatus img `shouldBe` ImageProcessed
            isJust (imgRawPath img) `shouldBe` True
      it "does nothing when the range begin has no raw" $ \config -> do
        let begin = mkImage config "a" "test" Nothing Nothing [simpleFile "a.jpg"] Nothing [] [] Nothing MediaImage def
            ranged = mkImage config "a_1" "test" Nothing Nothing [simpleFile "a_1.jpg"]
                             Nothing [] [] (Just ("a", "a_3")) MediaImage def
            dir = picDirWith "test" [begin, ranged]
        maybeUpdateStandaloneRange config dir ranged `shouldBe` Nothing
      it "rewrites standalone range images in the folder" $ \config -> do
        let root = simpleRawImage config
            ranged = mkImage config "a_1" "test" Nothing Nothing [simpleFile "a_1.jpg"]
                             Nothing [] [] (Just ("a", "a_3")) MediaImage def
            dir = resolveProcessedRanges config (picDirWith "test" [root, ranged])
        imgStatus (pdImages dir Map.! "a_1") `shouldBe` ImageProcessed
    describe "stats and folder class" $ do
      it "counts sizes and files across image kinds" $ \config -> do
        let raw = mkImage config "a" "t" (Just $ fileSized "a.nef" 100) Nothing [] Nothing [] [] Nothing MediaImage def
            standalone = mkImage config "b" "t" Nothing Nothing [fileSized "b.jpg" 40] Nothing [] [] Nothing MediaImage def
            processed = mkImage config "c" "t" (Just $ fileSized "c.nef" 50) Nothing [fileSized "c.jpg" 20] Nothing [] [] Nothing MediaImage def
            orphaned = mkImage config "d" "t" Nothing (Just $ fileSized "d.xmp" 5) [] Nothing [] [] Nothing MediaImage def
            movie = mkImage config "e" "t" Nothing Nothing [] (Just $ fileSized "e.mov" 80) [] [] Nothing MediaMovie def
            untracked = mkImage config "f" "t" Nothing Nothing [] Nothing [] [fileSized "f.other" 7] Nothing MediaUnknown def
            stats = foldl' updateStatsWithPic zeroStats [raw, standalone, processed, orphaned, movie, untracked]
        sRaw stats `shouldBe` 1
        sStandalone stats `shouldBe` 1
        sProcessed stats `shouldBe` 1
        sOrphaned stats `shouldBe` 1
        sMovies stats `shouldBe` 1
        sUntracked stats `shouldBe` 1
        totalStatsCount stats `shouldBe` 6
        totalStatsSize stats `shouldBe` 100 + 40 + 50 + 20 + 5 + 80 + 7
        sRawSize stats `shouldBe` 150
      it "sums two stats structures" $ \_ -> do
        let a = zeroStats { sRaw = 1, sRawSize = 10 }
            b = zeroStats { sStandalone = 2, sStandaloneSize = 5 }
            s = sumStats a b
        sRaw s `shouldBe` 1
        sStandalone s `shouldBe` 2
        totalStatsSize s `shouldBe` 15
      it "maps stats onto folder classes" $ \_ -> do
        folderClassFromStats zeroStats `shouldBe` FolderEmpty
        folderClassFromStats (zeroStats { sRaw = 1 }) `shouldBe` FolderRaw
        folderClassFromStats (zeroStats { sRaw = 1, sProcessed = 1 }) `shouldBe` FolderUnprocessed
        folderClassFromStats (zeroStats { sStandalone = 1 }) `shouldBe` FolderStandalone
        folderClassFromStats (zeroStats { sStandalone = 1, sProcessed = 1 }) `shouldBe` FolderMixed
        folderClassFromStats (zeroStats { sOrphaned = 1 }) `shouldBe` FolderMixed
        folderClassFromStats (zeroStats { sProcessed = 1 }) `shouldBe` FolderProcessed
        folderClassFromStats (zeroStats { sMovies = 1 }) `shouldBe` FolderProcessed
      it "computes folderClass from images" $ \config -> do
        folderClass (createTestPicDir "empty") `shouldBe` FolderEmpty
        folderClass ((createTestPicDir "empty") { pdUntracked = [simpleFile "SHA1SUMS"] })
          `shouldBe` FolderEmpty
        folderClass (picDirWith "raw" [simpleRawImage config]) `shouldBe` FolderRaw
    describe "queries" $ do
      it "filters images by class" $ \config -> do
        let raw = simpleRawImage config
            jpeg = mkImage config "b" "test" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def
            dir = picDirWith "test" [raw, jpeg]
            repo = mkRepository (Map.singleton "test" dir)
        map imgName (filterImagesByClass [ImageUnprocessed] repo) `shouldBe` ["a"]
        map imgName (filterImagesByClass [ImageStandalone] repo) `shouldBe` ["b"]
      it "omits folder-level untracked files from allRepoFiles" $ \config -> do
        let dir = (picDirWith "test" [simpleRawImage config])
                    { pdUntracked = [simpleFile "SHA1SUMS"] }
            repo = mkRepository (Map.singleton "test" dir)
        map fileName (allRepoFiles repo) `shouldBe` ["a.nef"]
      it "reports image file kinds" $ \config -> do
        let raw = simpleRawImage config
            movie = mkImage config "m" "t" Nothing Nothing [] (Just $ simpleFile "m.mov") [] [] Nothing MediaMovie def
            untracked = simpleUntrackedImage config "t" "u"
        imageHasImages raw `shouldBe` True
        imageHasMovies raw `shouldBe` False
        imageHasMovies movie `shouldBe` True
        imageHasUntracked untracked `shouldBe` True
        imageHasUntracked raw `shouldBe` False
        null (allImageFiles raw) `shouldBe` False
        null (allViewableImageFiles raw) `shouldBe` False
      it "counts pictures in a folder" $ \config -> do
        let dir = picDirWith "test" [simpleRawImage config]
        numPics dir `shouldBe` 1
        numRawPics dir `shouldBe` 1
        hasViewablePics dir `shouldBe` False
        hasViewablePics (picDirWith "s" [mkImage config "b" "s" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def]) `shouldBe` True
      it "aggregates image problems" $ \config -> do
        let warned = (simpleRawImage config) { imgExif = def { exifWarning = Set.singleton "bad" } }
            clean = mkImage config "b" "test" Nothing Nothing [simpleFile "b.jpg"] Nothing [] [] Nothing MediaImage def
            dir = picDirWith "test" [warned, clean]
        imgProblems warned `shouldBe` Set.singleton "exif: bad"
        pdProblems dir Map.! Just "exif: bad" `shouldBe` 1
        pdProblems dir Map.! Nothing `shouldBe` 1
      it "builds time keys from exif dates" $ \config -> do
        let img = (simpleRawImage config) { imgExif = datedExif 2020 7 15 }
        imageYear img `shouldBe` Just 2020
        imageYearMonth img `shouldBe` Just (2020, 7)
        fst (imageTimeKey img) `shouldBe` Just (LocalTime (fromGregorian 2020 7 15) midnight)
      it "uses default orientation when the view file has none" $ \config ->
        transformParams (transformForImage (simpleRawImage config)) `shouldBe` transformParams def
      it "uses the jpeg orientation when present" $ \config -> do
        let f = (simpleFile "a.jpg") { fileExif = def { exifOrientation = OrientationRightTop } }
            img = mkImage config "a" "test" Nothing Nothing [f] Nothing [] [] Nothing MediaImage def
        transformParams (transformForImage img) `shouldBe` transformParams (affineTransform OrientationRightTop)
    describe "viewableAsIs" $ do
      it "matches configured viewable extensions" $ \config -> do
        viewableAsIs "photo.jpg" config `shouldBe` True
        viewableAsIs "photo.png" config `shouldBe` True
        viewableAsIs "photo.nef" config `shouldBe` False
  withContext $ do
    describe "addDirToRepo event merge" $ do
      let implicitEv = Just GrandVacationEvent
            { eventName = "trip"
            , eventPeople = []
            , eventSource = EventImplicit implicitDateRangeDesc
            }
          explicitEv = Just BirthdayEvent
            { eventName = "trip"
            , eventPeople = []
            , eventSource = EventExplicit (Just "corydalis.yaml")
            }
          picDirnoEvent = createTestPicDir "trip"
          picDirImplicitEvent = picDirnoEvent { pdEvent = implicitEv }
          picDirExplicitEvent = picDirnoEvent { pdEvent = explicitEv }
      it "keeps an event when merging a new folder into one without" $ \ctx -> do
        let mergedImpl = addDirToRepo (ctxConfig ctx) picDirImplicitEvent (Map.singleton "trip" picDirnoEvent)
            mergedExpl = addDirToRepo (ctxConfig ctx) picDirExplicitEvent (Map.singleton "trip" picDirnoEvent)
        pdEvent (mergedImpl Map.! "trip") `shouldBe` implicitEv
        pdEvent (mergedExpl Map.! "trip") `shouldBe` explicitEv
      it "keeps an event when merging a folder without one into one with" $ \ctx -> do
        let mergedImpl = addDirToRepo (ctxConfig ctx) picDirnoEvent (Map.singleton "trip" picDirImplicitEvent)
            mergedExpl = addDirToRepo (ctxConfig ctx) picDirnoEvent (Map.singleton "trip" picDirExplicitEvent)
        pdEvent (mergedImpl Map.! "trip") `shouldBe` implicitEv
        pdEvent (mergedExpl Map.! "trip") `shouldBe` explicitEv
      it "prefers an explicit event over an implicit one" $ \ctx -> do
        let mergedNew = addDirToRepo (ctxConfig ctx) picDirExplicitEvent (Map.singleton "trip" picDirImplicitEvent)
            mergedOld = addDirToRepo (ctxConfig ctx) picDirImplicitEvent (Map.singleton "trip" picDirExplicitEvent)
        pdEvent (mergedNew Map.! "trip") `shouldBe` explicitEv
        pdEvent (mergedOld Map.! "trip") `shouldBe` explicitEv
    describe "search cache" $ do
      it "caches a search result" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
        getSearchResults ctx m1 [] `shouldReturn` m1
        getSearchResults ctx (error "Failed to cache") [] `shouldReturn` m1
      it "evaluates the results before caching them" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
            unevaluated = (Map.singleton ("a", (Nothing, "b")) (error "deep"),
                           Map.empty)
        getSearchResults ctx unevaluated [] `shouldThrow` anyErrorCall
        -- Nothing was cached.
        getSearchResults ctx m1 [] `shouldReturn` m1
      it "flushes the search cache when a scan's status changes" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
            m2 = (Map.empty, Map.empty)
        serial <- atomically $ newRepo (ctxRepo ctx)
        getSearchResults ctx m1 [] `shouldReturn` m1
        -- A failing scan only changes the status.
        _ <- runScanAction ctx serial (throwString "scan exploded")
        getSearchResults ctx m2 [] `shouldReturn` m2
      it "flushes the search cache on rescan" $ \ctx -> do
        let image = simpleRawImage (ctxConfig ctx)
            m1 = (Map.singleton ("a", (Nothing, "b")) image,
                  Map.singleton "a" image)
            m2 = (Map.empty, Map.empty)
        launchScanFileSystem ctx
        _ <- waitForScan ctx
        getSearchResults ctx m1 [] `shouldReturn` m1
        getSearchResults ctx m2 [] `shouldReturn` m1
        launchScanFileSystem ctx
        _ <- waitForScan ctx
        getSearchResults ctx m2 [] `shouldReturn` m2
    describe "countImagesBy" $ do
      it "counts images by attribute, the most frequent first" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = [ simpleUntrackedImage config f n
                   | (f, n) <- [("a", "1"), ("b", "1"), ("b", "2"), ("c", "1")
                               , ("c", "2"), ("c", "3")] ]
        countImagesBy (Just . imgParent) imgs `shouldBe`
          [(3, "c"), (2, "b"), (1, "a")]
      it "orders equal counts by descending attribute" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = map (simpleUntrackedImage config "f") ["x", "y", "z"]
        countImagesBy (Just . imgName) imgs `shouldBe`
          [(1, "z"), (1, "y"), (1, "x")]
      it "skips images without the attribute" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = map (simpleUntrackedImage config "f") ["x", "y"]
            attr i = if imgName i == "x" then Nothing else Just (imgName i)
        countImagesBy attr imgs `shouldBe` [(1, "y")]
        countImagesBy (const (Nothing :: Maybe Int)) imgs `shouldBe` []
    describe "imageDateRange" $ do
      let dated config name date =
            exifImage config "f" name (\e -> e { exifCreateDate = date })
          day = localDay . zonedTimeToLocalTime . etTime
          dates = fmap (\((d1, a1), (d2, a2)) -> (day d1, a1, day d2, a2))
      it "finds the earliest and the latest dated images" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = [ dated config "b" (Just (exifDate 2021 1 1))
                   , dated config "a" (Just (exifDate 2019 1 1))
                   , dated config "n" Nothing
                   , dated config "c" (Just (exifDate 2020 1 1))
                   ]
        dates (imageDateRange imgName imgs) `shouldBe`
          Just (fromGregorian 2019 1 1, "a", fromGregorian 2021 1 1, "b")
      it "orders images with the same date by attribute" $ \ctx -> do
        let config = ctxConfig ctx
            imgs = [ dated config n (Just (exifDate 2021 1 1)) | n <- ["y", "x", "z"] ]
        fmap (\((_, a1), (_, a2)) -> (a1, a2)) (imageDateRange imgName imgs)
          `shouldBe` Just ("x", "z")
      it "is the same image for a single dated image" $ \ctx -> do
        let imgs = [dated (ctxConfig ctx) "x" (Just (exifDate 2021 1 1))]
        fmap (\((_, a1), (_, a2)) -> (a1, a2)) (imageDateRange imgName imgs)
          `shouldBe` Just ("x", "x")
      it "is nothing without dated images" $ \ctx -> do
        let imgs = [dated (ctxConfig ctx) "x" Nothing]
        dates (imageDateRange imgName imgs) `shouldBe` Nothing
        dates (imageDateRange imgName []) `shouldBe` Nothing
    describe "imageNeighbours" $ do
      let results config =
            let mk (f, n) = simpleUntrackedImage config f n
                imgs = map mk [("a", "1"), ("b", "1"), ("b", "2"), ("b", "3"), ("c", "1")]
                fldrs = map mk [("a", "1"), ("b", "1"), ("c", "1")]
            in ( Map.fromList [((imgParent i, imageTimeKey i), i) | i <- imgs]
               , Map.fromList [(imgParent i, i) | i <- fldrs])
          names = fmap (\i -> (imgParent i, imgName i))
          namesOf n = ( names (Just (nbFirst n)), names (nbPrevFolder n)
                      , names (nbPrev n), names (nbNext n)
                      , names (nbNextFolder n), names (Just (nbLast n)))
      it "finds all neighbours of an image in the middle" $ \ctx -> do
        let config = ctxConfig ctx
            img = simpleUntrackedImage config "b" "2"
        namesOf (imageNeighbours (results config) img) `shouldBe`
          ( Just ("a", "1"), Just ("a", "1"), Just ("b", "1"), Just ("b", "3")
          , Just ("c", "1"), Just ("c", "1"))
      it "crosses folder boundaries for the previous and next images" $ \ctx -> do
        let config = ctxConfig ctx
            n = imageNeighbours (results config) (simpleUntrackedImage config "b" "1")
            n' = imageNeighbours (results config) (simpleUntrackedImage config "b" "3")
        names (nbPrev n) `shouldBe` Just ("a", "1")
        names (nbNext n') `shouldBe` Just ("c", "1")
      it "has no previous items for the first image" $ \ctx -> do
        let config = ctxConfig ctx
            img = simpleUntrackedImage config "a" "1"
        namesOf (imageNeighbours (results config) img) `shouldBe`
          ( Just ("a", "1"), Nothing, Nothing, Just ("b", "1")
          , Just ("b", "1"), Just ("c", "1"))
      it "has no next items for the last image" $ \ctx -> do
        let config = ctxConfig ctx
            img = simpleUntrackedImage config "c" "1"
        namesOf (imageNeighbours (results config) img) `shouldBe`
          ( Just ("a", "1"), Just ("b", "1"), Just ("b", "3"), Nothing
          , Nothing, Just ("c", "1"))
      it "orders the images of a folder by date, then by name" $ \ctx -> do
        let config = ctxConfig ctx
            dated name y = exifImage config "d" name (\e -> e { exifCreateDate = fmap (\y' -> exifDate y' 1 1) y })
            -- Names in the opposite order of the dates; undated first.
            imgs = [dated "1" (Just 2021), dated "2" (Just 2020), dated "3" Nothing]
            res = (Map.fromList [((imgParent i, imageTimeKey i), i) | i <- imgs], Map.empty)
            n = imageNeighbours res (dated "2" (Just 2020))
        names (nbPrev n) `shouldBe` Just ("d", "3")
        names (nbNext n) `shouldBe` Just ("d", "1")
        names (Just (nbFirst n)) `shouldBe` Just ("d", "3")
        names (Just (nbLast n)) `shouldBe` Just ("d", "1")
      it "finds the neighbours of an image missing from the results" $ \ctx -> do
        let config = ctxConfig ctx
            n = imageNeighbours (results config) (simpleUntrackedImage config "b" "15")
        namesOf n `shouldBe`
          ( Just ("a", "1"), Just ("a", "1"), Just ("b", "1"), Just ("b", "2")
          , Just ("c", "1"), Just ("c", "1"))
      it "uses the image itself as first and last for a single result" $ \ctx -> do
        let img = simpleUntrackedImage (ctxConfig ctx) "a" "1"
            single = ( Map.singleton (imgParent img, imageTimeKey img) img
                     , Map.singleton (imgParent img) img)
        imageNeighbours single img `shouldBe`
          ImageNeighbours img Nothing Nothing Nothing Nothing img
      it "uses the image itself as first and last for empty results" $ \ctx -> do
        let img = simpleUntrackedImage (ctxConfig ctx) "a" "1"
        imageNeighbours (Map.empty, Map.empty) img `shouldBe`
          ImageNeighbours img Nothing Nothing Nothing Nothing img
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
      it "does not try to render untracked-only files" $ \ctx -> do
        repo <- getRepo ctx
        let config = ctxConfig ctx
            untracked = mkImage config "sums" "folder" Nothing Nothing [] Nothing []
                          [simpleFile "SHA1SUMS"] Nothing MediaUnknown def
            dirs = addImageToRepo config (repoDirs repo) untracked
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
              return repo { repoDirs = foldl' (addImageToRepo config) Map.empty pictures }
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
            dirs = foldl' (addImageToRepo config) (repoDirs repo) (map picture ["x", "y", "z"])
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
    describe "upToDateCacheFile" $ do
      it "gives the time of an up to date cache file, from one look" $ \ctx -> do
        let path = cfgCacheDir (ctxConfig ctx) </> "cached"
        upToDateCacheFile path 0 `shouldReturn` Nothing
        writeFile path ("" :: ByteString)
        -- Empty files are never valid images.
        upToDateCacheFile path 0 `shouldReturn` Nothing
        writeNonEmpty path
        -- The time of its last change (as filePathLastTouch).
        touched <- filePathLastTouch path
        upToDateCacheFile path 0 `shouldReturn` Just touched
        -- Older than its source.
        upToDateCacheFile path 4e9 `shouldReturn` Nothing
    describe "imageViewSource" $ do
      it "picks the file an image is viewed from, and how" $ \ctx -> do
        let config = ctxConfig ctx
            nef = simpleFile "r.nef"
            png = simpleFile "r.png"
            jpg = simpleFile "r.jpg"
            mov = simpleFile "m.mov"
            image raw jpegs movie =
              mkImage config "r" "folder" raw Nothing jpegs movie [] [] Nothing
                MediaImage
            source = fmap (first fileName) . imageViewSource config
        source (image (Just nef) [jpg] Nothing def) `shouldBe` Just ("r.jpg", ViewAsIs)
        source (image (Just nef) [] Nothing def) `shouldBe` Just ("r.nef", ViewEmbedded)
        source (image (Just png) [] Nothing def) `shouldBe` Just ("r.png", ViewAsIs)
        source (image (Just nef) [] Nothing def { flagsSoftMaster = True })
          `shouldBe` Just ("r.nef", ViewAsIs)
        source (image Nothing [] (Just mov) def) `shouldBe` Just ("m.mov", ViewMovie)
        source (simpleUntrackedImage config "folder" "u") `shouldBe` Nothing
    describe "imagePendingSizes" $ do
      let noLog _ _ = return ()
          rawFile ctx = (simpleFile "r.nef") { fileParent = mkSym (pack $ sourceDir ctx) }
          rawImage ctx raw = mkImage (ctxConfig ctx) "r" "folder" (Just raw) Nothing []
                               Nothing [] [] Nothing MediaImage def
          movieFile ctx = (simpleFile "m.mov") { fileParent = mkSym (pack $ outputDir ctx) }
          movieImage ctx m = mkImage (ctxConfig ctx) "m" "folder" Nothing Nothing []
                               (Just m) [] [] Nothing MediaMovie def
          pendingOf ctx img = do
            dirs <- resolveStorageDirs noLog (ctxConfig ctx)
            imagePendingSizes noLog (ctxConfig ctx) dirs img (autoSizeList ctx)
          shouldPend ctx img expected = pendingOf ctx img >>= \case
            Right sizes -> sizes `shouldBe` expected
            Left err    -> expectationFailure $ "unexpected error: " ++ show err
      it "leaves extracting a missing preview or frame to the build" $ \ctx -> do
        let config = ctxConfig ctx
            raw = rawFile ctx
            movie = movieFile ctx
        -- Neither is valid, so an extraction would fail; but none is
        -- tried, and all the sizes are pending.
        forM_ [ (raw, rawImage ctx raw), (movie, movieImage ctx movie) ] $ \(f, img) -> do
          writeFile (fileFullPath f) ("not a picture" :: ByteString)
          shouldPend ctx img (autoSizeList ctx)
          doesFileExist (embeddedImagePath config (fileFullPath f)) `shouldReturn` False
      it "checks the previews against an up to date extracted preview" $ \ctx -> do
        let config = ctxConfig ctx
            raw = rawFile ctx
            path = fileFullPath raw
            sizes = autoSizeList ctx
        writeFile path ("raw" :: ByteString)
        writeNonEmpty (embeddedImagePath config path)
        shouldPend ctx (rawImage ctx raw) sizes
        mapM_ (writeNonEmpty . scaledImagePath config path) sizes
        shouldPend ctx (rawImage ctx raw) []
      it "rebuilds everything from a stale extracted preview" $ \ctx -> do
        let config = ctxConfig ctx
            -- Changed after the extracted preview and previews below.
            raw = (rawFile ctx) { fileMTime = 4e9 }
            path = fileFullPath raw
            sizes = autoSizeList ctx
        writeFile path ("raw" :: ByteString)
        writeNonEmpty (embeddedImagePath config path)
        mapM_ (writeNonEmpty . scaledImagePath config path) sizes
        shouldPend ctx (rawImage ctx raw) sizes
      it "refuses an image without a viewable file" $ \ctx -> do
        res <- pendingOf ctx (simpleUntrackedImage (ctxConfig ctx) "folder" "u")
        case res of
          Left ImageNotViewable -> return ()
          other -> expectationFailure $ "expected ImageNotViewable, got " ++ show other
      it "doesn't rebuild an up to date raw file" $ \ctx -> do
        repo <- getRepo ctx
        let config = ctxConfig ctx
            raw = rawFile ctx
            path = fileFullPath raw
            sizes = autoSizeList ctx
            dirs = addImageToRepo config (repoDirs repo) (rawImage ctx raw)
            repo' = repo { repoDirs = dirs }
        writeFile path ("raw" :: ByteString)
        writeNonEmpty (embeddedImagePath config path)
        mapM_ (writeNonEmpty . scaledImagePath config path) sizes
        pg <- forceBuildThumbCaches ctx repo'
        pgNoop pg `shouldBe` length sizes
        pgGoal pg `shouldBe` 0
        pgNumErrors pg `shouldBe` 0
      it "counts an image that can't be checked as errors" $ \ctx -> do
        -- A picture outside the picture folders fails the storage checks.
        let jpeg = (jpegFile ctx) { fileParent = mkSym (pack . cfgCacheDir $ ctxConfig ctx) }
        writeFile (fileFullPath jpeg) ("" :: ByteString)
        (repo, img) <- insertJpeg ctx jpeg
        pendingOf ctx img >>= \case
          Left (ImageError _) -> return ()
          other -> expectationFailure $ "expected the check to fail, got " ++ show other
        pg <- forceBuildThumbCaches ctx repo
        pgGoal pg `shouldBe` length (autoSizeList ctx)
        pgNoop pg `shouldBe` 0
        pgNumErrors pg `shouldBe` length (autoSizeList ctx)
      it "rejects a picture replaced by a symbolic link" $ \ctx -> do
        let jpeg = jpegFile ctx
            secret = cfgCacheDir (ctxConfig ctx) </> "secret.jpg"
        writeFile secret ("" :: ByteString)
        createSymbolicLink secret (fileFullPath jpeg)
        (repo, img) <- insertJpeg ctx jpeg
        pendingOf ctx img >>= \case
          Left (ImageError _) -> return ()
          other -> expectationFailure $ "expected the check to fail, got " ++ show other
        pg <- forceBuildThumbCaches ctx repo
        pgNumErrors pg `shouldBe` length (autoSizeList ctx)
      it "builds from an already extracted preview or frame" $ \ctx -> do
        let config = ctxConfig ctx
            raw = rawFile ctx
            movie = movieFile ctx
        forM_ [ (raw, rawImage ctx raw), (movie, movieImage ctx movie) ] $ \(f, img) -> do
          let epath = embeddedImagePath config (fileFullPath f)
          writeFile (fileFullPath f) ("not a picture" :: ByteString)
          writeNonEmpty epath
          -- The extracted file is up to date, so it is used as is.
          res <- imageAtRes (ctxRenderToolLimits ctx) noLog config img Nothing
          case res of
            Right (built, mime, path) -> do
              built `shouldBe` False
              mime `shouldBe` "image/jpeg"
              path `shouldBe` epath
            Left err -> expectationFailure $ "unexpected error: " ++ show err
      it "counts a failed extraction as an error of the build" $ \ctx -> do
        repo <- getRepo ctx
        let config = ctxConfig ctx
            movie = movieFile ctx
            dirs = addImageToRepo config (repoDirs repo) (movieImage ctx movie)
            repo' = repo { repoDirs = dirs }
        writeFile (fileFullPath movie) ("not a movie" :: ByteString)
        pg <- forceBuildThumbCaches ctx repo'
        pgGoal pg `shouldBe` length (autoSizeList ctx)
        pgNoop pg `shouldBe` 0
        pgNumErrors pg `shouldBe` length (autoSizeList ctx)
    describe "ensureStorageFile" $ do
      let isChangedError (ImageError msg) =
            "changed since the last scan" `isInfixOf` msg
          isChangedError _ = False
      it "accepts a file within the picture folders" $ \ctx -> do
        let jpeg = jpegFile ctx
        writeFile (fileFullPath jpeg) ("" :: ByteString)
        ensureStorageFile (\_ _ -> return ()) (ctxConfig ctx) jpeg
      it "rejects, and logs, a file replaced by a symbolic link" $ \ctx -> do
        let jpeg = jpegFile ctx
            path = fileFullPath jpeg
            secret = cfgCacheDir (ctxConfig ctx) </> "secret.txt"
        writeFile secret ("" :: ByteString)
        createSymbolicLink secret path
        logs <- newIORef []
        let logfn level msg = modifyIORef logs ((level, fromLogStr msg) :)
        ensureStorageFile logfn (ctxConfig ctx) jpeg
          `shouldThrow` isChangedError
        logged <- readIORef logs
        case logged of
          [(level, msg)] -> do
            level `shouldBe` LevelWarn
            msg `shouldSatisfy` (encodeUtf8 (pack path) `isInfixOf`)
            msg `shouldSatisfy` ("has changed since the last scan" `isInfixOf`)
          _ -> expectationFailure $ "Expected one log message, got: " ++
                 show logged
    describe "movieToServe" $ do
      let noLog _ _ = return ()
          movieFile ctx = (simpleFile "a.mp4")
            { fileParent = mkSym (pack $ outputDir ctx) }
          withMovies ctx movs =
            (mkImage (ctxConfig ctx) "a" "folder" Nothing Nothing
               [jpegFile ctx] Nothing [] [] Nothing MediaImage def)
              { imgMovs = movs }
      it "returns the movie file" $ \ctx -> do
        let movie = movieFile ctx
        writeFile (fileFullPath movie) ("" :: ByteString)
        result <- movieToServe noLog (ctxConfig ctx) (withMovies ctx [movie])
        either (Left . show) Right result `shouldBe` Right movie
      it "rejects images without movies" $ \ctx -> do
        result <- movieToServe noLog (ctxConfig ctx) (withMovies ctx [])
        case result of
          Left ImageNotViewable -> return ()
          _ -> expectationFailure $ "Unexpected result: " ++ show result
      it "rejects a movie replaced by a symbolic link" $ \ctx -> do
        let movie = movieFile ctx
            secret = cfgCacheDir (ctxConfig ctx) </> "secret.txt"
        writeFile secret ("" :: ByteString)
        createSymbolicLink secret (fileFullPath movie)
        result <- movieToServe noLog (ctxConfig ctx) (withMovies ctx [movie])
        case result of
          Left (ImageError _) -> return ()
          _ -> expectationFailure $ "Unexpected result: " ++ show result
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
