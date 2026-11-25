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

import qualified Data.ByteString.Lazy as BSL
import           Data.Default
import           Data.Either      (isLeft, isRight)
import qualified Data.Set         as Set
import qualified Control.Exception
import           Control.Exception (AsyncException (ThreadKilled))
import           Data.Time.Clock.POSIX (POSIXTime)
import           Control.Concurrent.STM.TSem (signalTSem, waitTSem)
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   doesFileExist, listDirectory, renameFile)
import           System.Exit      (ExitCode (..))
import           System.FilePath  (splitFileName, takeDirectory, takeFileName)
import           System.Posix.Files (createNamedPipe, createSymbolicLink)
import           Control.Monad.Logger (LogLevel (..))
import           System.Log.FastLogger (fromLogStr)
import           System.Posix.IO   (OpenMode (..), closeFd, defaultFileFlags,
                                   openFd)
import qualified System.IO
import           System.Process.Typed (proc, readProcess, setCloseFds)

import           Cache            (cacheTempDir)
import           Exif
import           Pics
import           Repository
import           Repository.Scan
import           Repository.State
import           Utils.Storage      (resolveStorageDirs)
import           Utils.Tools        (decodeToolOutput, readToolProcess)
import           TestImport

-- | A modification time far in the future (in 2096), newer than any test
-- file, for cache files to always be outdated. After that year, the
-- tests using it will fail and it will need to be updated.
farFutureTime :: POSIXTime
farFutureTime = 4000000000

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

spec :: Spec
spec = parallel $ do
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

    describe "transformForImage" $ do
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

  withUnscannedContext $ do
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
            dirs = addImageToRepo (repoDirs repo) (rawImage ctx raw)
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
        let movie = movieFile ctx
            dirs = addImageToRepo (repoDirs repo) (movieImage ctx movie)
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
