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
import           Data.Either      (isLeft)
import qualified Data.Map         as Map
import qualified Data.Set         as Set
import           Data.Time        (LocalTime (..), ZonedTime (..), midnight,
                                   utc)
import           Data.Time.Clock.POSIX (POSIXTime)
import           System.Directory (createDirectoryIfMissing, doesFileExist,
                                   listDirectory, renameFile)
import           System.Exit      (ExitCode (..))
import           System.FilePath  (splitFileName, takeDirectory)
import           System.Posix.Files (createNamedPipe, createSymbolicLink,
                                     ownerReadMode, ownerWriteMode,
                                     unionFileModes)
import           Control.Monad.Logger (LogLevel (..))
import           System.Log.FastLogger (fromLogStr)
import           System.Process.Typed (proc, readProcess)

import           AtomTypes
import           Exif
import           Pics
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

-- | Writes a real JPEG image, using ImageMagick.
writeRealJpeg :: FilePath -> IO ()
writeRealJpeg path = do
  (code, _, err) <- readProcess (proc "convert" ["-size", "8x8", "xc:white", "jpeg:" ++ path])
  unless (code == ExitSuccess) $
    expectationFailure ("Failed to create test JPEG: " ++ show err)

-- | Writes a JPEG image large enough to be accepted as an embedded one.
writePreviewJpeg :: FilePath -> IO ()
writePreviewJpeg path = do
  (code, _, err) <- readProcess (proc "convert" ["-size", "128x128", "gradient:red-blue",
                                                 "jpeg:" ++ path])
  unless (code == ExitSuccess) $
    expectationFailure ("Failed to create test JPEG: " ++ show err)

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
  try $ loadCachedOrBuild config path path decoder "image/jpeg" 0
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
buildPreview config path =
  try $ loadCachedOrBuild config path path (Right "jpeg") "image/jpeg" 0 previewSize

-- | The path of the preview built by 'buildPreview'.
previewPath :: Config -> FilePath -> IO FilePath
previewPath config path =
  case findBestSize previewSize (cfgAllImageSizes config) of
    Nothing  -> expectationFailure "expected a cached size for 1920" >> return ""
    Just res -> return $ scaledImagePath config path res

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
  describe "ImageMagick policy" $ withConfig $ do
    it "is used by the test configuration" $ \config ->
      cfgMagickPolicyDir config `shouldBe` Just "config/magick"
    it "is loaded by ImageMagick" $ \config -> do
      requireConvert
      checkMagickPolicy config `shouldReturn` Right ()
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
    let largeEnough = BSL.replicate 1024 0
    it "starts all JPEG images with the JPEG magic" $
      withConfig' $ \config -> do
        requireConvert
        let path = cfgCacheDir config </> "real.jpg"
        writeRealJpeg path
        contents <- readFile path
        jpegMagic `BSL.isPrefixOf` fromStrict contents `shouldBe` True
    it "accepts JPEG images" $ \_ -> do
      let image = jpegMagic <> largeEnough
      checkEmbeddedImage "PreviewImage" ExitSuccess image "" `shouldBe` Right image
    it "refuses images which are not JPEGs" $ \_ ->
      checkEmbeddedImage "PreviewImage" ExitSuccess ("\x89PNG\r\n" <> largeEnough) ""
        `shouldBeLeftWithMessage` "PreviewImage is not a JPEG"
    it "refuses too small images" $ \_ ->
      checkEmbeddedImage "PreviewImage" ExitSuccess jpegMagic ""
        `shouldBeLeftWithMessage` "PreviewImage is too small (3 bytes)"
    it "reports a missing embedded image" $ \_ ->
      checkEmbeddedImage "PreviewImage" ExitSuccess "" ""
        `shouldBeLeftWithMessage` "No embedded image PreviewImage found"
    it "returns the error of a failed extraction" $ \_ -> do
      let result = checkEmbeddedImage "PreviewImage" (ExitFailure 1)
                     (jpegMagic <> largeEnough) "tool error"
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
        result <- extractEmbedded config path 0 "PreviewImage"
        case result of
          Left msg -> expectationFailure ("Failed to extract the preview: " ++ show msg)
          Right (mime, extracted) -> do
            mime `shouldBe` "image/jpeg"
            expected <- readFile preview
            readFile extracted `shouldReturn` asByteString expected
    it "fails if there is no embedded image" $
      withConfig' $ \config -> do
        requireExifTool
        requireConvert
        let preview = cfgCacheDir config </> "preview.jpg"
            path = cfgCacheDir config </> "photo.mie"
        writePreviewJpeg preview
        writeFileWithPreview preview path
        result <- extractEmbedded config path 0 "JpgFromRaw"
        result `shouldBeLeftWithMessage` "No embedded image JpgFromRaw found"
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
            extractEmbedded config path 0 "PreviewImage" >>= \case
              Left msg -> expectationFailure ("Failed to extract the preview: " ++ show msg)
              Right (_, extracted) -> do
                writeFile extracted ("old" :: ByteString)
                _ <- extractEmbedded config path mtime "PreviewImage"
                readFile extracted `shouldReturn`
                  (if rebuilt then expected else "old")
  describe "movie frames" $ withConfig $ do
    it "extracts the first frame as a JPEG" $ \config -> do
      requireFfmpeg
      let path = cfgCacheDir config </> "movie.mp4"
      writeMovie path
      result <- extractFirstFrame config path 0
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
      extractFirstFrame config path 0 >>= \case
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
      result <- extractFirstFrame config path 0
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
      result <- extractFirstFrame config path 0
      result `shouldSatisfy` isLeft
      doesFileExist (embeddedImagePath config path) `shouldReturn` False
    it "leaves no file behind on failure" $ \config -> do
      requireFfmpeg
      let dir = cfgCacheDir config </> "sub"
          path = dir </> "broken.mp4"
      createDirectoryIfMissing True dir
      writeFile path ("not a movie" :: ByteString)
      extractFirstFrame config path 0 >>= (`shouldSatisfy` isLeft)
      let (outDir, _) = splitFileName (embeddedImagePath config path)
      outputs <- listDirectory outDir
      outputs `shouldSatisfy` all (\f -> not ("broken.mp4-" `isPrefixOf` f))
    it "refuses movies without a configured demuxer" $ \config -> do
      let path = cfgCacheDir config </> "movie.xyz"
      writeFile path ("whatever" :: ByteString)
      result <- extractFirstFrame config path 0
      result `shouldBeLeftWithMessage` "No movie demuxer configured for extension 'xyz'"
    forM_ [(False, "reuses an up to date", 0), (True, "rebuilds an outdated", farFutureTime)] $
      \(rebuilt, what, mtime) ->
        it (what ++ " frame") $ \config -> do
          requireFfmpeg
          let path = cfgCacheDir config </> "movie.mp4"
          writeMovie path
          extractFirstFrame config path 0 >>= \case
            Left msg -> expectationFailure ("Failed to extract the frame: " ++ show msg)
            Right (_, frame) -> do
              writeFile frame ("old" :: ByteString)
              _ <- extractFirstFrame config path mtime
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
  describe "external tools" $ do
    it "returns an error if a tool cannot be run" $ \_ -> do
      result <- readToolProcess (proc "corydalis-no-such-tool" [])
      -- The error names the tool, to make the failure easy to diagnose.
      result `shouldSatisfy` either ("corydalis-no-such-tool" `isInfixOf`) (const False)
    it "decodes invalid UTF-8 in tool output without failing" $ \_ -> do
      -- 0xff is never valid in UTF-8; forcing the text must not throw.
      text <- evaluate . force $ decodeToolOutput "bad \xff name: no such file"
      text `shouldBe` "bad \xfffd name: no such file"
    it "returns the exit code and output of a tool" $ \_ -> do
      result <- readToolProcess (proc "sh" ["-c", "echo out; echo err >&2; exit 3"])
      result `shouldBe` Right (ExitFailure 3, "out\n", "err\n")
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
        writeViaTempFile (target config) $ \tmp ->
          writeFile tmp ("contents" :: ByteString)
        readFile (target config) `shouldReturn` ("contents" :: ByteString)
        entries (target config) `shouldReturn` ["file"]
      it "writes to another file, in the same directory" $ \config -> do
        writeViaTempFile (target config) $ \tmp -> do
          takeDirectory tmp `shouldBe` takeDirectory (target config)
          tmp `shouldNotBe` target config
          doesFileExist (target config) `shouldReturn` False
          writeFile tmp ("x" :: ByteString)
      it "replaces an existing file" $ \config -> do
        writeNonEmpty (target config)
        writeViaTempFile (target config) $ \tmp ->
          writeFile tmp ("new" :: ByteString)
        readFile (target config) `shouldReturn` ("new" :: ByteString)
      it "leaves nothing behind on failure, and rethrows" $ \config -> do
        let failing tmp = do
              writeFile tmp ("partial" :: ByteString)
              throwIO (ImageError "conversion failed")
        writeViaTempFile (target config) failing
          `shouldThrow` (\case ImageError msg -> msg == "conversion failed"
                               _ -> False)
        entries (target config) `shouldReturn` []
  withConfig $ do
    describe "NFData" $ do
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
  withUnscannedContext $ do
    describe "scan abort" $ do
      it "records synchronous scan failures as RepoError" $ \ctx -> do
        repo <- getRepo ctx
        result <- runScanAction ctx repo (throwString "scan exploded")
        case repoStatus result of
          RepoError msg -> msg `shouldSatisfy` ("scan exploded" `isInfixOf`)
          other         -> expectationFailure $ "expected RepoError, got " ++ show other
        repo' <- getRepo ctx
        case repoStatus repo' of
          RepoError msg -> msg `shouldSatisfy` ("scan exploded" `isInfixOf`)
          other         -> expectationFailure $ "expected RepoError, got " ++ show other
      it "stops scan work when the scanner thread is cancelled" $ \ctx -> do
        started <- newEmptyMVar
        block <- newEmptyMVar
        cleaned <- newIORef False
        repo <- getRepo ctx
        scanner <- async $ runScanAction ctx repo $
          (putMVar started () >> takeMVar block >> return repo)
          `finally` writeIORef cleaned True
        takeMVar started
        cancel scanner
        readIORef cleaned `shouldReturn` True
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
    describe "scanSubDir" $ do
      it "loads date-named folders and skips others" $ \ctx -> do
        let level1 = sourceDir ctx </> "level1"
        touchFile (level1 </> "2024-01-01-trip" </> "a.nef")
        touchFile (level1 </> "not-a-date" </> "a.nef")
        dirs <- scanSubDir ctx level1 True
        map pdName dirs `shouldMatchList` ["2024-01-01-trip"]
      it "reports symbolic links to folders as scan errors" $ \ctx -> do
        let level1 = sourceDir ctx </> "level1"
        touchFile (level1 </> "2024-01-01-trip" </> "a.nef")
        createSymbolicLink (level1 </> "2024-01-01-trip")
          (level1 </> "2024-01-02-link")
        dirs <- scanSubDir ctx level1 True
        map pdName dirs `shouldMatchList` ["2024-01-01-trip"]
        errs <- map (peItem &&& peError) . pgErrors <$> getProgress ctx
        errs `shouldBe`
          [(pack $ level1 </> "2024-01-02-link", "symbolic link, not followed")]
    describe "scanBaseDir" $ do
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
