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

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Previews and viewing: building the cached previews of images and
-- movies, and choosing the file to serve for an image.
module Pics
  ( ImageError(..)
  , ImageSize(..)
  , fileLastTouch
  , cacheFileEndings
  , imagePendingSizes
  , loadCachedOrBuild
  , checkMagickPolicy
  , bestMovie
  , ensureStorageFile
  , imageAtRes
  , movieToServe
  , transformForFile
  , transformForImage
#ifdef TEST
  , filePathLastTouch
  , findBestSize
  , embeddedImagePath
  , scaledImagePath
  , cacheFileNeedsBuild
  , upToDateCacheFile
  , scaledCacheNeedsBuild
  , buildPreviewFile
  , PreviewFormat(..)
  , previewArgs
  , checkPreviewImage
  , checkPreviewFile
  , tryImageError
  , writeViaTempFile
  , convertProcess
  , jpegMagic
  , pngMagic
  , checkEmbeddedImage
  , embeddedArgs
  , extractEmbedded
  , viewableAsIs
  , firstFrameArgs
  , extractFirstFrame
  , imageDecoder
  , movieDemuxer
  , ViewKind(..)
  , imageViewSource
#endif
  ) where

import           Control.Applicative
import qualified Data.ByteString         as BS (hGet)
import qualified Data.ByteString.Lazy    as BSL (append, isPrefixOf, length, null,
                                                pack)
import           Data.Semigroup
import qualified Data.Set                as Set
import qualified Data.Text               as Text
import qualified Data.Text.Short         as TS
import           Data.Time.Clock.POSIX
import           System.Directory        hiding (isSymbolicLink)
import qualified System.Environment
import qualified System.IO
import           System.Exit
import           System.FilePath
import           System.Log.FastLogger   (toLogStr)
import qualified System.Posix.Files      (fileSize)
import           System.Posix.Files      hiding (fileSize)
import           System.Posix.IO         (OpenFileFlags (..), OpenMode (..),
                                          closeFd, defaultFileFlags,
                                          fdToHandle, openFd)
import           System.Process.Typed
import           UnliftIO.Exception


import           Cache
import           Compat.Orphans          ()
import           Exif
import           Import.NoFoundation     hiding (fileName, fileSize, path)
import           Utils.Tools
import           Utils.Storage           (ResolvedStorageDirs,
                                          isCanonicalStoragePath,
                                          isCanonicalStoragePathIn)

import           Repository

data ImageError = ImageNotViewable
                | ImageError Text
  deriving (Show, Typeable)

instance Exception ImageError

-- | Represents a scaled down image size.
newtype ImageSize = ImageSize Int

-- | Return the last time a file has been touched (latest of ctime/mtime).
fileLastTouch :: File -> POSIXTime
fileLastTouch f = fileMTime f `max` fileCTime f

-- | Retun the last time a path on disk has been touched.
filePathLastTouch :: FilePath -> IO POSIXTime
filePathLastTouch p = do
  stat <- getSymbolicLinkStatus p
  return $ modificationTimeHiRes stat `max` statusChangeTimeHiRes stat

-- | The endings of the cache files' names in use: "-" and the suffix of
-- the configured sizes, the embedded images (and movie frames), and the
-- exif caches (see 'cachedBasename'). The dash is included, so that it
-- isn't added again for each cache file the cleanup checks.
--
-- A new kind of cache file must be added here, as the cache cleanup
-- deletes the files with any other suffix. The repository cache (see
-- "Repository.Scan") isn't, as it is at the root of the cache
-- directory, which the cleanup doesn't visit.
cacheFileEndings :: Config -> [Text]
cacheFileEndings config =
  map ("-" <>) $
    "embedded" :
    map (Text.pack . show) (Set.toList (cfgAllImageSizes config)) ++
    ["exif", "bexif"]

-- | Computes optimal image preview size.
--
-- Assuming the passed list is sorted, the function computes the
-- smallest size that is larger or equal than the required image
-- size. If none is found, it means we're requesting a size larger
-- than any of our allowed-to-be-cached sizes, so return Nothing.
findBestSize :: ImageSize -> Set Int -> Maybe Int
findBestSize (ImageSize i) = Set.lookupLE i

embeddedImagePath :: Config -> FilePath -> String
embeddedImagePath config path =
  cachedBasename config path "embedded"

scaledImagePath :: Config -> FilePath -> Int -> String
scaledImagePath config path res =
  cachedBasename config path (show res)

-- | Whether a cache file is missing, empty, or older than the source mtime.
--
-- An empty file is never a valid image. Such files were left behind by
-- failed conversions (before previews were written via a temporary
-- file), and by convert succeeding without writing the file it was
-- given (before previews were checked, see 'buildPreviewFile'); they
-- are rebuilt.
--
-- A file that can't be checked at all (e.g. as its name is too long,
-- or its parent directory is a regular file) also needs building, so
-- that the build reports the problem, the way its callers report
-- errors, instead of the check throwing.
cacheFileNeedsBuild :: FilePath -> POSIXTime -> IO Bool
cacheFileNeedsBuild fpath mtime = isNothing <$> upToDateCacheFile fpath mtime

-- | The last time an up to date cache file was touched (see
-- 'filePathLastTouch'), or 'Nothing' if it needs building (see
-- 'cacheFileNeedsBuild'), from a single look at the file, so that it
-- can't change in between.
upToDateCacheFile :: FilePath -> POSIXTime -> IO (Maybe POSIXTime)
upToDateCacheFile fpath mtime = do
  stat <- tryIO $ getFileStatus fpath
  return $ case stat of
    Right st | modificationTimeHiRes st >= mtime,
               System.Posix.Files.fileSize st /= 0 ->
      Just $ modificationTimeHiRes st `max` statusChangeTimeHiRes st
    _ -> Nothing

-- | Whether a scaled preview for this request size needs generating.
--
-- Snaps @size@ through 'findBestSize' onto 'cfgAllImageSizes' so the
-- check uses the same cache path as 'loadCachedOrBuild'. For auto
-- sizes (already in that set) the snap is identity. Returns 'False'
-- when no cached size applies (request smaller than every configured
-- size).
scaledCacheNeedsBuild :: Config -> FilePath -> POSIXTime -> ImageSize -> IO Bool
scaledCacheNeedsBuild config origPath mtime size =
  case findBestSize size (cfgAllImageSizes config) of
    Nothing  -> return False
    Just res -> cacheFileNeedsBuild (scaledImagePath config origPath res) mtime

-- | The sizes, among the given ones, whose previews of this image need
-- (re)building, checking only the cache files.
--
-- No tool is run (hence no 'ToolLimits'): for a raw file or a movie
-- whose viewable version must be extracted (see 'imageViewSource'), a
-- missing or stale extracted file means all the sizes need building,
-- and the extraction is left to the build. Otherwise, each size needs
-- building if its preview is missing, or older than the viewable file.
--
-- The configured directories are given resolved (see
-- 'resolveStorageDirs'), as this checks many images in a row.
imagePendingSizes :: LogFn -> Config -> ResolvedStorageDirs -> Image -> [Int]
                  -> IO (Either ImageError [Int])
imagePendingSizes logfn config storageDirs img sizes = try $ do
  (f, kind) <- maybe (throwIO ImageNotViewable) return $ imageViewSource config img
  ensureStorageFileWith (isCanonicalStoragePathIn storageDirs config) logfn f
  let path = fileFullPath f
      staleSizes mtime =
        filterM (scaledCacheNeedsBuild config path mtime . ImageSize) sizes
  case kind of
    ViewAsIs -> staleSizes (fileLastTouch f)
    _ -> upToDateCacheFile (embeddedImagePath config path) (fileLastTouch f)
           >>= maybe (return sizes) staleSizes

-- | Generate a preview for an image.
--
-- In this context, a preview is a smaller version of an image, for
-- faster/optimised use on a lower-powered device (mobile, etc.). It
-- is done using imagemagick's convert tool.
--
-- TODO: add loging of failures.
-- TODO: improve path manipulation \/ concatenation.
-- The decoder is passed explicitly to convert, since otherwise
-- ImageMagick picks it based on the file contents, e.g. running a
-- PostScript or SVG document named @photo.jpg@ as such. If no decoder
-- is known for the file (the 'Left' case), the preview can't be built.
loadCachedOrBuild :: ToolLimits -> Config -> FilePath -> FilePath -> Either Text String
                  -> MimeType -> POSIXTime -> ImageSize
                  -> IO (Bool, MimeType, FilePath)
loadCachedOrBuild tools config origPath bytesPath decoder mime mtime size = do
  let res = findBestSize size (cfgAllImageSizes config)
  case res of
    Nothing -> return (False, mime, bytesPath)
    Just res' -> do
      let fpath = scaledImagePath config origPath res'
          kind = if res' <= cfgThumbnailSize config
                   then PreviewThumbnail
                   else PreviewScaled
      -- Note: this calls getFileStatus not getSymbolicLinkStatus
      -- since this is a file, not a directory, so symlinks to
      -- somewhere else are OK-ish; we do not recurse into the target
      -- directory.
      needsGen <- cacheFileNeedsBuild fpath mtime
      when needsGen $ do
        decoder' <- either (throwIO . ImageError) return decoder
        buildPreviewFile tools config kind fpath bytesPath
          =<< convertProcess config (previewArgs kind decoder' res')
      return (needsGen, previewMimeType kind, fpath)

-- | Builds a preview into the given cache file, running the given tool
-- with the picture as its standard input, and its standard output going
-- to a temporary file (see 'previewArgs').
--
-- The tool's slot is taken first (see 'withToolSlot'), so that the
-- builds waiting for one hold neither the temporary file nor the
-- picture open.
--
-- The temporary file is moved in place only if the tool succeeded and
-- the file starts with the signature of the expected format (see
-- 'checkPreviewImage'): a failed, empty or partial preview must not be
-- served. If the tool fails, or its preview is refused, its error
-- output is added to the error.
--
-- All failures are thrown as an 'ImageError', which is what the
-- callers expect: any other exception (e.g. if the temporary file
-- can't be created, as the picture's name is too long once the cache
-- suffix is added) would fail the request, or stop the rendering of all
-- the other pictures (see 'toImageError').
buildPreviewFile :: ToolLimits -> Config -> PreviewFormat
                 -> FilePath  -- ^ The cache file to build.
                 -> FilePath  -- ^ The picture.
                 -> ProcessConfig stdin stdout stderr  -- ^ The tool.
                 -> IO ()
buildPreviewFile tools config kind fpath input pconfig =
  either (throwIO . toImageError) return =<<
  tryAny (withToolSlot tools $ \slot -> writeViaTempFile config fpath $ \tmpPath -> do
    let openInput = openRegularFile input `catchIO`
          (throwIO . ImageError . sformat ("Cannot read the image to scale: " % shown))
    (exitCode, err) <- either (throwIO . ImageError) return =<<
      bracket openInput System.IO.hClose (\h ->
        runToolToFile slot tmpPath (setStdin (useHandleOpen h) pconfig))
    let output = Text.strip $ decodeToolOutput err
        withOutput msg | Text.null output = msg
                       | otherwise = msg <> ": " <> output
    unless (exitCode == ExitSuccess) . throwIO . ImageError . withOutput $
      sformat ("convert failed (" % shown % ")") exitCode
    checkPreviewFile kind tmpPath >>= either (throwIO . ImageError . withOutput) return)

-- | Opens a regular file for reading, refusing anything else.
--
-- The file is opened without waiting: opening a pipe for reading would
-- otherwise wait for a writer, with no time limit (e.g. if a picture
-- was replaced by one after the scan). Its type is then checked on the
-- open file itself, so that it can't be replaced in between.
--
-- The handle is in binary mode, as 'fdToHandle' makes it. Asynchronous
-- exceptions are masked throughout, so that the file descriptor is
-- closed exactly once: here if the handle wasn't made, and otherwise by
-- the handle, which owns it. (Opening and checking the file are system
-- calls, which can't be interrupted anyway, and don't wait.)
openRegularFile :: FilePath -> IO System.IO.Handle
openRegularFile path = mask_ $ do
  fd <- openFd path ReadOnly defaultFileFlags { nonBlock = True, cloexec = True }
  (do status <- getFdStatus fd
      unless (isRegularFile status) . throwIO . ImageError $
        sformat ("Cannot read the image to scale: " % string % " is not a regular file") path
      fdToHandle fd) `onException` closeFd fd

-- | The kinds of previews built by convert.
data PreviewFormat
  = PreviewThumbnail  -- ^ A thumbnail: a PNG, scaled down and padded
                      -- to a square with transparency.
  | PreviewScaled     -- ^ A scaled-down (never enlarged) image: a JPEG.
  deriving (Show, Eq)

-- | The ImageMagick coder for a preview's output.
--
-- These are the canonical coder names, as the ImageMagick policy lists
-- them: ImageMagick 6 checks the name as given, so e.g. @jpg@ would be
-- refused by a policy allowing only @JPEG@.
previewCoder :: PreviewFormat -> String
previewCoder PreviewThumbnail = "png"
previewCoder PreviewScaled    = "jpeg"

-- | The MIME type of a preview.
previewMimeType :: PreviewFormat -> MimeType
previewMimeType PreviewThumbnail = "image/png"
previewMimeType PreviewScaled    = "image/jpeg"

-- | The bytes a preview of the given kind starts with.
previewMagic :: PreviewFormat -> LByteString
previewMagic PreviewThumbnail = pngMagic
previewMagic PreviewScaled    = jpegMagic

-- | The convert arguments for building a preview, reading the picture
-- from the standard input, and writing the preview to the standard
-- output (see 'buildPreviewFile').
--
-- Only the first image of the input is used (the @[0]@): files can
-- hold several (e.g. the pages of a TIFF, the frames of an animated
-- WebP, or the images of a HEIC file, which ImageMagick returns with
-- the primary image first), and convert would otherwise try to write
-- each of them, to numbered files. (For an animated WebP, ImageMagick
-- still decodes all the frames, so the policy must not limit how many
-- images it reads from a file; see config\/magick\/policy.xml.)
--
-- No path is named on the command line, so that convert has none to
-- interpret: it expands format specifiers in file names (e.g. the @%d@
-- of a picture named @100%d.jpg@, or of a folder name), in the output
-- name, and, once an image is selected, in the input name too; and
-- for several images, it writes numbered files next to the output file
-- it was given, not that file. Telling it to use the names as given
-- (@filename:literal@) is no fix, as ImageMagick 7.1.2 then also takes
-- the @[0]@ as part of the name.
--
-- The standard input and output are named as @\/dev\/fd\/0@ and
-- @\/dev\/fd\/1@, which convert opens as ordinary files. Not @-@: with
-- it, convert first copies the whole input to a temporary file (left
-- behind if it is killed). The output must be a file, not a pipe, to
-- which convert fails to write. @\/dev\/stdout@ would work too, but was
-- seen as a regular file, not a link, in a container whose
-- @\/dev\/fd@ was fine. systemd's @PrivateDevices@ keeps @\/dev\/fd@.
--
-- @-thumbnail@ records the name of its input in the thumbnail (its
-- @Thumb::URI@ property); here, it only names @\/dev\/fd\/0@, but it
-- is removed anyway, as it would disclose the picture's path to anyone
-- who can see the thumbnails if a path were ever given.
previewArgs :: PreviewFormat  -- ^ The kind of preview.
            -> String         -- ^ The decoder for the input.
            -> Int            -- ^ The size to scale down to (if larger).
            -> [String]
previewArgs kind decoder size =
  [decoder ++ ":/dev/fd/0[0]"] ++ operators ++ [previewCoder kind ++ ":/dev/fd/1"]
  where geom = show size ++ "x" ++ show size ++ ">"
        operators = case kind of
          PreviewThumbnail -> ["-thumbnail", geom, "-background", "none",
                               "-gravity", "center", "-extent", geom,
                               "+set", "Thumb::URI"]
          PreviewScaled    -> ["-resize", geom]

-- | Checks a preview written by convert, from its first bytes.
--
-- A safeguard: convert has been seen to succeed without writing the
-- file it was given (for several images, before only the first was
-- used, see 'previewArgs'); an empty preview, or one that doesn't
-- start with the signature of the format it is served as, must not be
-- cached.
checkPreviewImage :: PreviewFormat
                  -> LByteString  -- ^ The preview's first bytes (as many
                                  -- as its 'previewMagic' has, or all of
                                  -- them if it is shorter).
                  -> Either Text ()
checkPreviewImage kind start
  | BSL.null start = Left "Building the preview produced no image"
  | not (previewMagic kind `BSL.isPrefixOf` start) =
      Left $ sformat ("Building the preview produced something other than a " %
                      string % " image") (toUpper $ previewCoder kind)
  | otherwise = Right ()

-- | Checks the preview written by convert to the given file (see
-- 'checkPreviewImage').
checkPreviewFile :: PreviewFormat -> FilePath -> IO (Either Text ())
checkPreviewFile kind path =
  checkPreviewImage kind . snd <$> readFileStart path (BSL.length $ previewMagic kind)

-- | The 'ImageError' for a failure to build a cache file: the exception
-- itself if it is one, or else the exception, shown after \"Failed to
-- write the cache file: \" (e.g. if the temporary file couldn't be
-- created).
toImageError :: SomeException -> ImageError
toImageError e =
  fromMaybe (ImageError $ sformat ("Failed to write the cache file: " % shown) e)
            (fromException e)

-- | Runs an action writing a cache file, returning the message of its
-- failure, if any (see 'toImageError').
--
-- Only synchronous exceptions are caught ('tryAny'), so that e.g. a
-- cancelled request still stops the action.
tryImageError :: IO () -> IO (Either Text ())
tryImageError action = do
  result <- tryAny action
  return $ case result of
    Left e -> Left $ case toImageError e of
      ImageError msg   -> msg
      ImageNotViewable -> "The image is not viewable"
    Right () -> Right ()

-- | Returns the size of a file and its first bytes: the given number of
-- them, or the whole file if it is shorter.
readFileStart :: FilePath -> Int64 -> IO (Integer, LByteString)
readFileStart path n =
  System.IO.withBinaryFile path System.IO.ReadMode $ \h ->
    (,) <$> System.IO.hFileSize h
        <*> (fromStrict <$> BS.hGet h (fromIntegral n))

-- | Creates a cache file by writing it to a temporary file in the
-- cache's temporary directory (see 'cacheTempDir'), then renaming it to
-- its final name, so that the file only appears once fully written.
-- Both directories are created first, if needed; as the file is
-- renamed, it must be on the cache directory's filesystem.
--
-- The temporary file is named after the start of the file's name (its
-- first 50 characters, at most 200 bytes), which shows what is being
-- written, while leaving room for the random part added to it: with
-- the whole name, a file whose name is valid, but near the length
-- limit, couldn't be written.
--
-- If the writing action fails, the temporary file is removed, and the
-- exception is rethrown. The file gets the default permissions (subject
-- to the umask), as a newly created file would; if it replaces an
-- existing file, it does not keep that file's permissions (the cache
-- files are all created by this code, so they have the default ones).
writeViaTempFile :: Config -> FilePath -> (FilePath -> IO ()) -> IO ()
writeViaTempFile config path write = do
  let (dir, name) = splitFileName path
      tmpDir = cacheTempDir config
  createDirectoryIfMissing True dir
  createDirectoryIfMissing True tmpDir
  bracketOnError
    (do (tmpPath, h) <- System.IO.openTempFileWithDefaultPermissions tmpDir
                          (take 50 name ++ ".tmp")
        System.IO.hClose h
        return tmpPath)
    (void . tryIO . removeFile)
    (\tmpPath -> write tmpPath >> renameFile tmpPath path)

-- | Builds the process configuration for running ImageMagick's convert.
--
-- If a policy directory is configured, ImageMagick is pointed to it via
-- the MAGICK_CONFIGURE_PATH environment variable. convert gets none of
-- Corydalis' other open files.
convertProcess :: Config -> [String] -> IO (ProcessConfig () () ())
convertProcess config args = do
  let pconfig = setCloseFds True $ proc "convert" args
      var = "MAGICK_CONFIGURE_PATH"
  case cfgMagickPolicyDir config of
    Nothing -> return pconfig
    Just dir -> do
      env <- System.Environment.getEnvironment
      return $ setEnv ((var, dir) : filter ((/= var) . fst) env) pconfig

-- | Checks that ImageMagick uses the configured policy.
--
-- ImageMagick silently ignores a policy it can't find, so this checks
-- that the policy file exists, and that ImageMagick lists it among the
-- policies it loaded. Without a configured policy directory, there is
-- nothing to check.
checkMagickPolicy :: Config -> IO (Either Text ())
checkMagickPolicy config =
  case cfgMagickPolicyDir config of
    Nothing -> return $ Right ()
    Just dir -> do
      let policyFile = dir </> "policy.xml"
          failWith = return . Left
      found <- doesFileExist policyFile
      if not found
        then failWith $ sformat ("ImageMagick policy file " % string % " not found") policyFile
        else do
          expected <- canonicalizePath policyFile
          pconfig <- convertProcess config ["-list", "policy"]
          result <- readToolProcess pconfig
          case result of
            Left msg -> failWith msg
            Right (ExitSuccess, out, _) -> do
              let loaded = [ Text.unpack (Text.strip path)
                           | line <- Text.lines (decodeToolOutput out)
                           , Just path <- [Text.stripPrefix "Path:" (Text.strip line)] ]
              loaded' <- mapM canonicalizePath loaded
              return $ if expected `elem` loaded'
                then Right ()
                else Left $ sformat ("ImageMagick did not load the policy file " % string) policyFile
            Right (_, out, err) ->
              failWith $ "Failed to list the ImageMagick policies: " <>
                decodeToolOutput (err `BSL.append` out)

-- | Ordered list of tags that represent embedded images.
previewTags :: [String]
previewTags = ["JpgFromRaw", "PreviewImage"]

-- | Minimum thumbnail size to consider it a valid image.
minImageSize :: Int64
minImageSize = 512

-- | The bytes every JPEG image starts with.
--
-- A JPEG stream must start with the start of image (SOI) marker, @FF
-- D8@, followed by another marker (e.g. an application or table
-- segment); all markers, and the fill bytes allowed before them, start
-- with @FF@.
jpegMagic :: LByteString
jpegMagic = BSL.pack [0xFF, 0xD8, 0xFF]

-- | The bytes every PNG image starts with (its signature).
pngMagic :: LByteString
pngMagic = BSL.pack [0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A]

-- | Checks the result of extracting an embedded image with exiftool.
--
-- Succeeds if the extraction succeeded, and the image is a JPEG large
-- enough to be a real image. Exiftool already checks this for most,
-- but not all, embedded images it knows about. Note that exiftool
-- succeeds, without any output, if the image doesn't have the
-- requested embedded image.
checkEmbeddedImage :: String       -- ^ The embedded image's tag.
                   -> ExitCode     -- ^ Exiftool's exit code.
                   -> Int64        -- ^ The image's size.
                   -> LByteString  -- ^ The image's first bytes (at least
                                   -- as many as 'jpegMagic' has).
                   -> LByteString  -- ^ Exiftool's error output.
                   -> Either Text ()
checkEmbeddedImage tag exitCode size start err
  | exitCode /= ExitSuccess =
      Left $ sformat ("Failed to extract the embedded image " % string % " (" %
                      shown % "): " % stext) tag exitCode (decodeToolOutput err)
  | size == 0 =
      Left $ sformat ("No embedded image " % string % " found") tag
  | size < minImageSize =
      Left $ sformat ("The embedded image " % string % " is too small (" % int %
                      " bytes)") tag size
  | not (jpegMagic `BSL.isPrefixOf` start) =
      Left $ sformat ("The embedded image " % string % " is not a JPEG") tag
  | otherwise = Right ()

-- | The exiftool arguments for extracting the given embedded image.
--
-- The path comes after @--@, so that it is never taken as an option,
-- even if it starts with a dash (the paths we pass are absolute, but
-- this makes it independent of that, as for the scan).
embeddedArgs :: String -> FilePath -> [String]
embeddedArgs tag path = ["-b", "-" ++ tag, "--", path]

-- | Extracts and saves an embedded thumbnail from an image.
--
-- The extracted image is required to be a JPEG, otherwise it is not
-- saved, and an error is returned. A previously extracted image is
-- reused, unless it is older than the given modification time of the
-- source file.
extractEmbedded :: ToolLimits -> Config -> FilePath -> POSIXTime -> String
                -> IO (Either Text (MimeType, FilePath))
extractEmbedded tools config path mtime tag = do
  let outputPath = embeddedImagePath config path
      args = embeddedArgs tag path
      mime = "image/jpeg"
      goodRet = Right (mime, outputPath)
  needsBuild <- cacheFileNeedsBuild outputPath mtime
  if needsBuild
    then do
      -- Exiftool writes the image straight to a temporary file, moved
      -- in place only if it is a valid image; otherwise, or if the
      -- extraction is interrupted, it is removed, so that no partial
      -- or invalid image is served as the preview.
      let pconfig =
            setStdin closed
            . setCloseFds True
            $ proc "exiftool" args
      result <- tryImageError . withToolSlot tools $ \slot ->
        writeViaTempFile config outputPath $ \tmpPath -> do
          ran <- runToolToFile slot tmpPath pconfig
          (exitCode, err) <- either (throwIO . ImageError) return ran
          (size, start) <- readFileStart tmpPath (BSL.length jpegMagic)
          either (throwIO . ImageError) return $
            checkEmbeddedImage tag exitCode (fromIntegral size) start err
      return (const goodRet =<< result)
    else
      return goodRet

-- | Extract the first valid thumbnail from an image.
bestEmbedded :: ToolLimits -> Config -> FilePath -> POSIXTime -> IO (Either Text (MimeType, FilePath))
bestEmbedded tools config path mtime =
  go config path previewTags
  where go _ _ [] = return $ Left "No tags available"
        go c p (x:xs) = do
          r <- extractEmbedded tools c p mtime x
          case r of
            Left _ -> case xs of
              [] -> return r
              _  -> go c p xs
            Right _ -> return r

-- | Checks is a file is directly viewable.
viewableAsIs :: ShortText -> Config -> Bool
viewableAsIs (TS.toText -> path) (cfgViewableImages -> exts)  =
  any (`Text.isSuffixOf` path) exts

-- | The ffmpeg arguments for extracting the first frame of a movie, as
-- a JPEG.
--
-- The demuxer is passed explicitly, since otherwise ffmpeg guesses the
-- format from the file contents, and some formats (e.g. playlists, or
-- concat scripts) make it read other files; it also only reads local
-- files, and the "file:" prefix ensures the path is read as a file
-- name, even if it contains a colon.
firstFrameArgs :: String    -- ^ The demuxer (see 'movieDemuxer').
               -> FilePath  -- ^ The movie.
               -> FilePath  -- ^ The output file.
               -> [String]
firstFrameArgs demuxer path output = [
  "-nostdin",
  -- Only errors in its output, which is shown when the extraction
  -- fails: by default, ffmpeg also writes its banner, the details of
  -- the streams, and its progress.
  "-loglevel", "error",
  "-y",
  "-protocol_whitelist", "file",
  "-f", demuxer,
  "-i", "file:" ++ path,
  "-r", "1",
  "-frames", "1",
  "-f", "image2",
  -- Use the output name as given, not as a pattern (e.g. if it
  -- contains '%').
  "-update", "1",
  -- The output file has no extension, so ffmpeg would use the default
  -- codec of the muxer, which happens to be mjpeg; set it explicitly
  -- instead.
  "-c:v", "mjpeg",
  output
  ]

-- | Extracts and saves the first frame from a movie, as a JPEG.
--
-- As for embedded images, a previously extracted frame is reused,
-- unless it is older than the given modification time of the movie.
extractFirstFrame :: ToolLimits -> Config -> FilePath -> POSIXTime
                  -> IO (Either Text (MimeType, FilePath))
extractFirstFrame tools config path mtime =
  case movieDemuxer config path of
    Left msg -> return $ Left msg
    Right demuxer -> do
      let outputPath = embeddedImagePath config path
          mime = "image/jpeg"
          goodRet = Right (mime, outputPath)
      needsBuild <- cacheFileNeedsBuild outputPath mtime
      if needsBuild
        then do
          -- ffmpeg can leave a partial file behind when it fails (or is
          -- interrupted), so it writes to a temporary file, moved in
          -- place only on success.
          result <- tryImageError . withToolSlot tools $ \slot ->
            writeViaTempFile config outputPath $ \tmpPath -> do
              let pconfig =
                    setStdin closed
                    . setCloseFds True
                    $ proc "ffmpeg" (firstFrameArgs demuxer path tmpPath)
              ran <- runTool slot pconfig
              case ran of
                Left msg -> throwIO $ ImageError msg
                Right (exitCode, out, err) ->
                  unless (exitCode == ExitSuccess) .
                    throwIO . ImageError . decodeToolOutput $ out `BSL.append` err
          return (const goodRet =<< result)
        else
          return goodRet

-- | Compute a presumed jpeg's mime type.
jpegMimeType :: File -> MimeType
jpegMimeType = fileMimeType "image/jpeg"

-- | The ImageMagick decoder for a file, based on its extension.
imageDecoder :: Config -> FilePath -> Either Text String
imageDecoder config path =
  let ext = Text.toLower . Text.pack . drop 1 . takeExtension $ path
  in case lookup ext (cfgImageDecoders config) of
       Just decoder -> Right (Text.unpack decoder)
       Nothing -> Left $ sformat ("No image decoder configured for extension '" % stext %
                                  "' (see the imagedecoders setting)") ext

-- | The ffmpeg demuxer for a movie, based on its extension.
movieDemuxer :: Config -> FilePath -> Either Text String
movieDemuxer config path =
  let ext = Text.toLower . Text.pack . drop 1 . takeExtension $ path
  in case lookup ext (cfgMovieDemuxers config) of
       Just demuxer -> Right (Text.unpack demuxer)
       Nothing -> Left $ sformat ("No movie demuxer configured for extension '" % stext %
                                  "' (see the moviedemuxers setting)") ext

-- | The ImageMagick decoder for images extracted by Corydalis.
--
-- Both embedded previews and movie frames are extracted as JPEGs.
extractedDecoder :: Either Text String
extractedDecoder = Right "jpeg"

-- | Select best movie for an image.
bestMovie :: Image -> Maybe File
bestMovie img =
  case imgMovs img of
    m:_ -> Just m
    _   -> imgMasterMov img

-- | Ensures that a file can be read, i.e. it's still within the
-- configured picture directories, without symbolic links.
--
-- The scan skips symbolic links, but a file could have been replaced
-- by one (or one of its parent directories) since then.
--
-- The error shown to users is generic, since a rescan fixes it (the
-- file is then either used normally, or skipped and listed as a scan
-- error); the exact reason is logged.
ensureStorageFile :: LogFn -> Config -> File -> IO ()
ensureStorageFile logfn config =
  ensureStorageFileWith (isCanonicalStoragePath config) logfn

-- | Checks a file as 'ensureStorageFile' does, but with the given check
-- of its path (e.g. one with the configured directories resolved
-- beforehand, see 'isCanonicalStoragePathIn').
ensureStorageFileWith :: (FilePath -> IO Bool) -> LogFn -> File -> IO ()
ensureStorageFileWith isCanonical logfn f = do
  let path = fileFullPath f
  ok <- isCanonical path
  unless ok $ do
    logfn LevelWarn $ "File " <> toLogStr path <> " has changed since " <>
      "the last scan: it is no longer within the picture folders, or its " <>
      "path now contains a symbolic link"
    throwIO $ ImageError
      "The file has changed since the last scan; a rescan will update it."

-- | How the viewable version of an image is obtained from its file
-- (see 'imageViewSource').
data ViewKind
  = ViewAsIs     -- ^ The file itself (a JPEG, or a viewable raw file).
  | ViewEmbedded -- ^ A preview embedded in the (raw) file.
  | ViewMovie    -- ^ A movie's embedded preview, or else its first frame.
  deriving (Show, Eq)

-- | The file an image is viewed from, and how its viewable version is
-- obtained from it: a processed file is used directly; for a raw file,
-- its embedded preview is extracted (unless browsers can show it as it
-- is); for a movie, its embedded preview or first frame.
--
-- This only picks the file; 'getViewableVersion' does the extraction,
-- and 'imagePendingSizes' only checks what it would need.
imageViewSource :: Config -> Image -> Maybe (File, ViewKind)
imageViewSource config img
  | j:_ <- imgJpegPath img = Just (j, ViewAsIs)
  | Just j <- imgRawPath img,
    True <- flagsSoftMaster (imgFlags img) = Just (j, ViewAsIs)
  | Just r <- imgRawPath img =
      Just (r, if fileName r `viewableAsIs` config then ViewAsIs else ViewEmbedded)
  | Just m <- bestMovie img = Just (m, ViewMovie)
  | otherwise = Nothing

-- | Returns a viewable version of an image (see 'imageViewSource'),
-- extracting it if needed. Also returns the ImageMagick decoder to use
-- for the viewable file.
getViewableVersion :: ToolLimits -> LogFn -> Config -> Image
                   -> IO (File, FilePath, Either Text String, MimeType, POSIXTime)
getViewableVersion tools logfn config img = do
  (f, kind) <- maybe (throwIO ImageNotViewable) return $ imageViewSource config img
  ensureStorageFile logfn config f
  let path = fileFullPath f
      extracted = either (throwIO . ImageError) $ \(mime, epath) -> do
        -- The file can disappear meanwhile (e.g. removed from the cache
        -- directory by hand): a failure for this image only.
        mtime <- either (const . throwIO $ ImageError "The extracted preview can't be read") return
                   =<< tryIO (filePathLastTouch epath)
        return (f, epath, extractedDecoder, mime, mtime)
  case kind of
    ViewAsIs ->
      return (f, path, imageDecoder config path, jpegMimeType f, fileLastTouch f)
    ViewEmbedded ->
      extracted =<< bestEmbedded tools config path (fileLastTouch f)
    ViewMovie -> do
      embedded <- bestEmbedded tools config path (fileLastTouch f)
      extracted =<< case embedded of
        Left _  -> extractFirstFrame tools config path (fileLastTouch f)
        Right e -> return (Right e)

-- | Return the path (on the filesystem) to a specific size rendering of an image.
--
-- If there was no specific size requested, return the path to an
-- unscaled variant. If a size was requested, return the smallest
-- rendering at a size larger-or-equal than the requested one.
--
-- In case the image requested doesn't already have a valid jpeg
-- variant, try to extract an embedded preview from one of the raw
-- files, and use that as substitute full-size version, before scaling
-- down (as needed).
--
-- TODO: handle and meaningfully return errors.
imageAtRes :: ToolLimits -> LogFn -> Config -> Image -> Maybe ImageSize -> IO (Either ImageError (Bool, MimeType, FilePath))
imageAtRes tools logfn config img size = try $ do
  (origFile, path, decoder, mime, mtime) <- getViewableVersion tools logfn config img
  case size of
    Nothing -> return (False, mime, path)
    Just s  -> loadCachedOrBuild tools config (fileFullPath origFile) path decoder mime mtime s

-- | Returns the movie file to serve for an image, after checking that
-- it can be read (see 'ensureStorageFile').
movieToServe :: LogFn -> Config -> Image -> IO (Either ImageError File)
movieToServe logfn config img = try $
  case bestMovie img of
    Just f  -> f <$ ensureStorageFile logfn config f
    Nothing -> throwIO ImageNotViewable

transformForFile :: File -> Transform
transformForFile  = affineTransform . exifOrientation . fileExif

transformForImage :: Image -> Transform
transformForImage =
  maybe def transformForFile . fileToView

fileToView :: Image -> Maybe File
fileToView img =
  case imgJpegPath img of
    f:_ -> Just f
    []  -> imgRawPath img
