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
{-# LANGUAGE ViewPatterns      #-}

module TestImport
    ( module TestImport
    , module X
    , module TS
    ) where

import           Application                    (appMiddlewares, makeFoundation,
                                                 makeLogFn, makeLogWare)
import           ClassyPrelude                  as X hiding (Handler, delete,
                                                      deleteBy)
import           Database.Persist               as X hiding (get)
import           Database.Persist.Sql           (SqlPersistM,
                                                 runSqlPersistMPool)
import           Exif                           (Exif (..), ExifTime (..))
import           Foundation                     as X
import           Model                          as X
import           Repository                     (File (..), Image (..),
                                                 InodeInfo (..), MediaType (..),
                                                 PicDir (..), RepoDirs,
                                                 RepoStatus (..),
                                                 Repository (..),
                                                 buildGroupExif, buildTimeSort,
                                                 computeImagesStats,
                                                 computeRepoStats, mkImage,
                                                 repoGlobalExif,
                                                 startupRepository)
import           Repository.Build               (addDirToRepo)
import           Repository.Scan                (launchScanFileSystem, scanAll,
                                                 waitForScan)
import           Repository.State               (Ctx, getRepo,
                                                 initContext)
import           Test.Hspec                     as X
import           Test.Hspec.Expectations.Lifted as THL (shouldSatisfy)
import           Test.Hspec.QuickCheck          as X
import           Types                          as X
import           Yesod.Auth                     as X
import           Yesod.Core                     (ToContent (..))
import           Yesod.Core.Unsafe              (fakeHandlerGetLogger)
import           Yesod.Default.Config2          (loadYamlSettings,
                                                 makeYesodLogger, useEnv)
import           Yesod.Test                     as X

import qualified Control.Exception              as E
import qualified Data.ByteString                as BS
import qualified Data.ByteString.Char8          as BS8
import           Data.Bits                      (xor)
import           Data.Default
import           Data.Either
import qualified Data.Map                       as Map
import qualified Data.Set                       as Set
import qualified Data.Text                      as Text
import           Data.Text.Short                (ShortText)
import           Data.Time                      (LocalTime (..),
                                                 ZonedTime (..),
                                                 midnight,
                                                 utc)
import qualified Data.Text.Short                as TS
import           Formatting
import           Network.Wai.Test               (simpleHeaders)
import           System.Posix.Types             (FileOffset)
import           Control.Concurrent.STM.TSem    (waitTSem)
import           GHC.Conc                       (BlockReason (..),
                                                 ThreadId, ThreadStatus (..),
                                                 threadStatus, yield)
import           Settings                       (AppSettings (..),
                                                 newRenderToolLimits,
                                                 newScanToolLimits)
import           System.Directory               (createDirectory,
                                                 createDirectoryIfMissing,
                                                 findExecutable,
                                                 removeDirectoryRecursive)
import           System.FilePath                (takeDirectory)
import           System.IO.Temp
import           System.Log.FastLogger          (defaultBufSize,
                                                 newStdoutLoggerSet,
                                                 rmLoggerSet)
import           System.Process.Typed           (ExitCode (..), proc,
                                                 readProcess)
import           Data.ByteString.Builder        (toLazyByteString)
import           Yesod.Core.Types               (Content (..),
                                                 ErrorResponse (..),
                                                 HandlerContents (..),
                                                 HandlerData (..),
                                                 HandlerFor (..),
                                                 YesodRequest (..),
                                                 loggerSet)
import           Text.HTML.DOM                  (parseLBS)
import           Text.XML.Cursor                (attribute, fromDocument)

-- Helper functions for running tests.

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

-- | Runs a handler outside of a request, as 'runHandler', so as an
-- anonymous user (without access to storage paths, see
-- 'hasAccessToStoragePaths'), but with the given GET parameters.
--
-- It returns how the handler stopped early (e.g. with a redirect or
-- an error), if it did.
--
-- This skips the authorization of the route (see 'isAuthorized'): the
-- tests aren't built with the @public-site@ flag, so all routes need
-- logging in, but on a public site anonymous users can reach most of
-- them, and only the handlers' own checks apply.
runAnonymousHandler :: [(Text, Text)] -> Handler a
                    -> YesodExample App (Either HandlerContents a)
runAnonymousHandler params handler =
  runHandler . withGetParams $ try handler
  where withGetParams h = HandlerFor $ \hd ->
          let req = handlerRequest hd
          in unHandlerFor h hd { handlerRequest = req { reqGetParams = params } }

-- | Whether a handler run by 'runAnonymousHandler' was refused (403),
-- with a message containing the given text.
isPermissionDenied :: Text -> Either HandlerContents a -> Bool
isPermissionDenied txt (Left (HCError (PermissionDenied msg))) =
  txt `isInfixOf` msg
isPermissionDenied _ _ = False

-- | Where a handler run by 'runAnonymousHandler' redirected to, if it
-- did.
redirectTarget :: Either HandlerContents a -> Maybe Text
redirectTarget (Left (HCRedirect _ location)) = Just location
redirectTarget _                              = Nothing

-- | The page rendered by a handler run by 'runAnonymousHandler',
-- failing if it stopped early.
anonymousPage :: (ToContent c) => [(Text, Text)] -> Handler c
              -> YesodExample App Text
anonymousPage params handler =
  runAnonymousHandler params handler >>= \case
    Right c  -> liftIO . contentText $ toContent c
    Left err -> liftIO . throwIO . userError $ "Handler failed: " <> show err
  where contentText (ContentBuilder b _)    =
          return . decodeUtf8 . toStrict $ toLazyByteString b
        contentText (ContentDontEvaluate c) = contentText c
        contentText _                       =
          throwIO $ userError "Handler returned a stream or a file"

{-# ANN ignoringIOErrors ("HLint: ignore Evaluate"::String) #-}
-- | Adapted from temporary's code.
ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe =
  ioe `E.catch` (\e -> const (return ()) (e :: IOException))

-- | Loads the test settings.
loadSettings :: IO AppSettings
loadSettings =
  loadYamlSettings
    ["config/test-settings.yml"]
    []
    useEnv

-- | Creates needed dir paths in the config.
updateConfig :: FilePath -> AppSettings -> IO Config
updateConfig tempDir (appConfig -> config) = do
  let inTemp = (tempDir </>)
      rawDir = inTemp "raw"
      jpgDir = inTemp "jpg"
      cache  = inTemp "cache"
  createDirectory rawDir
  createDirectory jpgDir
  createDirectory cache
  return config { cfgCacheDir = cache
                , cfgSourceDirs = [rawDir]
                , cfgOutputDirs = [jpgDir]
                }

-- | Updates settings with valid config.
updateSettings :: FilePath -> AppSettings -> IO AppSettings
updateSettings tempDir settings = do
  config <- updateConfig tempDir settings
  return settings { appConfig = config }

-- Simple, plain temp dir definitions.

-- | Creates and returns a temporary directory.
setTempDir :: IO FilePath
setTempDir = do
  rootTempDir <- getCanonicalTemporaryDirectory
  createTempDirectory rootTempDir "corydalis-test"

-- | Cleans up a temporary directory.
cleanupTempDir :: FilePath -> IO ()
cleanupTempDir  = ignoringIOErrors . removeDirectoryRecursive

withTempDir :: SpecWith FilePath -> Spec
withTempDir = around (bracket setTempDir cleanupTempDir)

-- Config spec definitions.

-- | Builds and returns a valid config.
openTempConfig :: AppSettings -> IO (FilePath, Config)
openTempConfig settings = do
  tempDir <- setTempDir
  config <- updateConfig tempDir settings
  return (tempDir, config)

-- | Runs an action with a temporary context.
withConfig' :: (Config -> IO ()) -> IO ()
withConfig' action = do
  settings <- loadSettings
  bracket (openTempConfig settings) (cleanupTempDir . fst) (action . snd)

-- | Spec definition for with context.
withConfig :: SpecWith Config -> Spec
withConfig = around withConfig'

-- Context spec definitions.

-- | Runs an action with the application's log function (see
-- 'makeLogFn'), logging to standard output, as the application does.
withTestLogFn :: AppSettings -> (LogFn -> IO a) -> IO a
withTestLogFn settings action =
  bracket (newStdoutLoggerSet defaultBufSize >>= makeYesodLogger)
          -- Removing the logger set also flushes it.
          (rmLoggerSet . loggerSet)
          (action . makeLogFn (appLogLevel settings))

-- | Creates a context, with the configured tool limits.
newTestContext :: AppSettings -> Config -> LogFn -> STM Ctx
newTestContext settings config logfn = do
  renderTools <- newRenderToolLimits settings
  scanTools <- newScanToolLimits settings
  initContext config logfn renderTools scanTools

-- | Builds and returns a valid context.
openTempContext :: AppSettings -> LogFn -> IO (FilePath, Ctx)
openTempContext settings logfn = do
  tempDir <- setTempDir
  config <- updateConfig tempDir settings
  ctx <- atomically $ newTestContext settings config logfn
  launchScanFileSystem ctx
  _ <- waitForScan ctx
  return (tempDir, ctx)

-- | Runs an action with a temporary context.
withContext' :: (Ctx -> IO ()) -> IO ()
withContext' action = do
  settings <- loadSettings
  withTestLogFn settings $ \logfn ->
    bracket (openTempContext settings logfn) (cleanupTempDir . fst) (action . snd)

-- | Spec definition for with context.
withContext :: SpecWith Ctx -> Spec
withContext = around withContext'

openUnscannedContext :: AppSettings -> LogFn -> IO (FilePath, Ctx)
openUnscannedContext settings logfn = do
  tempDir <- setTempDir
  config <- updateConfig tempDir settings
  ctx <- atomically $ newTestContext settings config logfn
  return (tempDir, ctx)

withUnscannedContext' :: (Ctx -> IO ()) -> IO ()
withUnscannedContext' action = do
  settings <- loadSettings
  withTestLogFn settings $ \logfn ->
    bracket (openUnscannedContext settings logfn) (cleanupTempDir . fst) (action . snd)

withUnscannedContext :: SpecWith Ctx -> Spec
withUnscannedContext = around withUnscannedContext'

-- App spec definitions.

-- | Builds and returns a valid app.
--
-- The initial scan of the (empty) repository is run to completion first:
-- the app otherwise starts it on the first request that needs the
-- repository, and doesn't wait for it, so the pages would show the
-- repository in whatever state the scan reached (starting, scanning,
-- finished), and the scan could overwrite changes made by the tests
-- (e.g. 'injectImages'). Tests that need another state set it
-- explicitly.
openTempApp :: IO (FilePath, TestApp App)
openTempApp = openTempAppWith id

-- | Builds and returns a valid app, with changed settings.
openTempAppWith :: (AppSettings -> AppSettings) -> IO (FilePath, TestApp App)
openTempAppWith change = do
  tempDir <- setTempDir
  settings <- change <$> (loadSettings >>= updateSettings tempDir)
  foundation <- makeFoundation settings
  let ctx = appContext foundation
  _ <- scanAll ctx
  repo <- waitForScan ctx
  case repoStatus repo of
    RepoFinished {} -> return ()
    status          -> E.throwIO . userError $ "The initial scan failed: " ++ show status
  logWare <- liftIO $ makeLogWare foundation
  return (tempDir, (foundation, logWare . appMiddlewares settings))

-- | Runs an action with a temporary app.
withApp' :: (TestApp App -> IO ())  -> IO ()
withApp' action = bracket openTempApp (cleanupTempDir . fst) (action . snd)

-- | Spec definition for with app.
withApp :: SpecWith (TestApp App) -> Spec
withApp = around withApp'

-- | Spec definition for with app, with changed settings.
withAppSettings :: (AppSettings -> AppSettings) -> SpecWith (TestApp App)
                -> Spec
withAppSettings change =
  around $ bracket (openTempAppWith change) (cleanupTempDir . fst) . (. snd)

-- Helper test functions

-- | Fails the test if exiftool is not installed.
--
-- Corydalis cannot work without exiftool, so the tests require it too,
-- rather than being skipped and silently giving less coverage.
requireExifTool :: IO ()
requireExifTool =
  findExecutable "exiftool" >>=
    maybe (expectationFailure "exiftool is required for running the tests")
          (const (return ()))

-- | Fails the test if ImageMagick's convert is not installed.
--
-- As for exiftool, previews cannot be built without it, so the tests
-- for building them are not skipped.
requireConvert :: IO ()
requireConvert =
  findExecutable "convert" >>=
    maybe (expectationFailure "ImageMagick (convert) is required for running the tests")
          (const (return ()))

-- | Fails the test if ImageMagick can't write and read back a HEIC
-- image.
--
-- The tests create HEIC images, which needs libheif's HEVC encoder as
-- well as its decoder; on recent Debian and Ubuntu releases, these are
-- separate packages. (@convert -list format@ shows HEIC as readable
-- even without the decoder, so this tries both.)
requireHeic :: IO ()
requireHeic = X.withSystemTempDirectory "heic" $ \dir -> do
  let path = "heic:" ++ dir </> "test.heic"
      run args = (\(code, _, _) -> code == ExitSuccess) <$>
                   readProcess (proc "convert" args)
  written <- run ["-size", "64x64", "xc:red", path]
  readBack <- if written then run [path, "info:"] else return False
  unless readBack $
    expectationFailure "ImageMagick must write and read HEIC images for running the tests \
                       \(on recent Debian/Ubuntu releases: libheif-plugin-libde265 and \
                       \libheif-plugin-x265)"

-- | Fails the test if ffmpeg is not installed.
--
-- Corydalis works without it, but the tests for extracting movie frames
-- need it, and are not skipped.
requireFfmpeg :: IO ()
requireFfmpeg =
  findExecutable "ffmpeg" >>=
    maybe (expectationFailure "ffmpeg is required for running the tests")
          (const (return ()))

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    -- Needed in order to get a CSRF token.
    get $ AuthR LoginR
    token <- pageCsrfToken
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userName u
        setUrl $ AuthR $ PluginR "dummy" []
        addCsrfHeader token

-- | Returns the redirect target of the last response, as given (an
-- absolute URL; 'getLocation' only handles paths).
responseLocation :: YesodExample App (Maybe ByteString)
responseLocation = withResponse $ \response ->
  return $ lookup "Location" $ simpleHeaders response

-- | Tool limits for the tests (see 'ToolLimits'), the same as the
-- application's.
testToolLimits :: IO ToolLimits
testToolLimits = atomically $ newToolLimits defaultToolSlots defaultRenderToolTimeout

-- | Waits until the given thread is blocked in STM, e.g. waiting for a
-- tool slot (see 'withToolSlot'), failing if it finishes first. There
-- is a last resort time limit, in case it never blocks.
waitUntilBlockedInSTM :: ThreadId -> IO ()
waitUntilBlockedInSTM tid =
  timeout (30 * 1000000) loop >>=
    maybe (expectationFailure "The thread never blocked") return
  where loop = do
          status <- threadStatus tid
          case status of
            ThreadBlocked BlockedOnSTM -> return ()
            ThreadFinished -> expectationFailure "The thread didn't block"
            ThreadDied -> expectationFailure "The thread didn't block"
            _ -> yield >> loop

-- | Counts the free slots of the given tool limits, taking them all.
takeFreeSlots :: ToolLimits -> IO Int
takeFreeSlots tools = go 0
  where go n = do
          taken <- atomically $
            (waitTSem (tlSlots tools) >> return True) `orElse` return False
          if taken then go (n + 1 :: Int) else return n

-- | Returns the CSRF token from the meta tag of the last page, as the
-- frontend code does.
pageCsrfToken :: YesodExample App Text
pageCsrfToken = do
  metas <- htmlQuery "meta[name=csrf-token]"
  case concatMap (attribute "content" . fromDocument . parseLBS) metas of
    [token] -> return token
    found   -> liftIO $ E.throwIO $ userError $
                 "Expected one csrf-token meta tag, got: " ++ show found

-- | Sends the CSRF token as a header, as the frontend code does.
addCsrfHeader :: Text -> RequestBuilder App ()
addCsrfHeader = addRequestHeader . (,) "X-XSRF-TOKEN" . encodeUtf8

-- | Performs a request, sending the CSRF token from the last page.
performWithToken :: ByteString -> Route App -> YesodExample App ()
performWithToken method route = do
  token <- pageCsrfToken
  request $ do
    setMethod method
    setUrl route
    addCsrfHeader token

-- | Create a user.
createUser :: Text -> YesodExample App (Entity User)
createUser ident =
    runDB $ insertEntity User
        { userName = ident
        , userPassword = Nothing
        , userDeleted = False
        , userGeneration = 0
        }

login :: YesodExample App ()
login = checkLoginIs 200 HomeR

checkLoginSuccessful :: Route App -> YesodExample App ()
checkLoginSuccessful = checkLoginIs 200

checkLoginIs :: Int -> Route App -> YesodExample App ()
checkLoginIs result route = do
      userEntity <- createUser "foo"
      authenticateAs userEntity

      get route
      statusIs result

checkRouteIs :: Route App -> Int -> YesodExample App ()
checkRouteIs route result = do
     get route
     statusIs result

checkRouteIsWithParams :: Route App -> [(Text, Text)] -> Int -> YesodExample App ()
checkRouteIsWithParams route params result = do
     request $ do
       setMethod "GET"
       setUrl route
       mapM_ (uncurry addGetParam) params
     statusIs result

checkRoute :: Route App -> YesodExample App ()
checkRoute = (`checkRouteIs` 200)

checkNotFound :: Route App -> YesodExample App ()
checkNotFound = (`checkRouteIs` 404)

checkRedirect :: YesodExample App ()
checkRedirect = do
    redir <- followRedirect
    redir `THL.shouldSatisfy` isRight

followRedirectOK :: YesodExample App ()
followRedirectOK = do
    statusIs 303
    checkRedirect
    statusIs 200

-- Picture mocking functions
mkSym :: Text -> SymbolizedItem
mkSym = mkSymbolizedItem

mkInode :: FilePath -> InodeInfo
mkInode name = InodeInfo
  { inodeName = name
  , inodeDirs = []
  , inodeIsDir = False
  , inodeMTime = 0
  , inodeCTime = 0
  , inodeSize = 0
  }

touchFile :: FilePath -> IO ()
touchFile path = do
  createDirectoryIfMissing True (takeDirectory path)
  BS8.writeFile path ""

simpleFile :: Text -> File
simpleFile filename =
  File { fileName = TS.fromText filename, fileCTime = 0, fileMTime = 0, fileSize = 0, fileParent = mkSym "test", fileDirs = mkSym "", fileExif = def }

-- | Generates a picture that only hos untracked (orphaned) elements.
simpleUntrackedImage :: Config -> Text -> Text -> Image
simpleUntrackedImage config (TS.fromText -> parent) name =
  let untracked = simpleFile (sformat (stext % ".other") name)
  in mkImage config (ImageName (TS.fromText name)) parent Nothing Nothing []
             Nothing [] [untracked] Nothing MediaImage def

-- | A simple unprocessed (raw) image, whose raw file has the given exif
-- information (as changes to the default one).
--
-- The exif information is set on the raw file, since the image's own
-- is derived from its files (by 'mkImage', and again when
-- 'injectImages' adds more images to the same folder), so a direct
-- change would be lost.
exifImage :: Config -> Text -> Text -> (Exif -> Exif) -> Image
exifImage config parent name change =
  let raw = File { fileName = TS.fromText (name <> ".nef"), fileCTime = 0
                 , fileMTime = 0, fileSize = 0, fileParent = mkSym parent
                 , fileDirs = mkSym "", fileExif = change def }
  in mkImage config (ImageName (TS.fromText name)) (TS.fromText parent)
             (Just raw) Nothing [] Nothing [] [] Nothing MediaImage def

-- | The exif time of midnight (UTC) on the given date.
exifDate :: Integer -> Int -> Int -> ExifTime
exifDate y m d = ExifTime (ZonedTime (LocalTime (fromGregorian y m d) midnight) utc)

simpleRawImage :: Config -> Image
simpleRawImage config =
  let f = File { fileName = "a.nef", fileCTime = 0, fileMTime = 0, fileSize = 0, fileParent = mkSym "test", fileDirs = mkSym "", fileExif = def }
  in mkImage config "a" "test" (Just f) Nothing []
             Nothing [] [] Nothing MediaImage def

mkRepository :: RepoDirs -> Repository
mkRepository dirs =
  startupRepository { repoDirs = dirs
                    , repoStats = computeRepoStats dirs
                    , repoExif = repoGlobalExif dirs
                    }

-- | Create a minimal PicDir for testing
createTestPicDir :: ShortText -> PicDir
createTestPicDir name =
  PicDir { pdName = name
         , pdMainPath = "/test/" <> name
         , pdSecPaths = []
         , pdImages = Map.empty
         , pdTimeSort = Set.empty
         , pdUntracked = []
         , pdYear = Nothing
         , pdTimestamp = Nothing
         , pdExif = def
         , pdStats = def
         , pdEvent = Nothing
         }

-- | Creates a folder with (empty) files in the first output directory,
-- and rescans the repository. Returns the directory of the folder.
addScannedFolder :: ShortText -> [FilePath] -> YesodExample App FilePath
addScannedFolder name files = do
  liftIO requireExifTool
  ctx <- appContext <$> getTestYesod
  -- Folders are two levels below the base directory; the name of the
  -- intermediate level is not used.
  let dir = fromMaybe (error "No output directory")
              (headMay (cfgOutputDirs (ctxConfig ctx))) </> "2020" </>
              TS.toString name
  liftIO $ do
    -- Wait for the initial scan, so that it doesn't miss the new folder.
    _ <- waitForScan ctx
    createDirectoryIfMissing True dir
    mapM_ (\f -> writeFile (dir </> f) ("" :: ByteString)) files
    _ <- launchScanFileSystem ctx
    repo <- waitForScan ctx
    case repoStatus repo of
      RepoError err -> expectationFailure $ "The scan failed: " ++ unpack err
      _             -> return ()
  return dir

-- | The exif warning of 'problemImage', i.e. its problem.
problemWarning :: (IsString a) => a
problemWarning = "Bad format in /srv/pictures/a.nef"

-- | An image with a problem (see 'problemWarning'): the
-- 'simpleRawImage', folder @test@ and name @a@.
problemImage :: Config -> Image
problemImage config =
  let img = simpleRawImage config
  in img { imgExif = (imgExif img) { exifWarning = Set.singleton problemWarning } }

-- | Injects the 'problemImage', once the initial scan (which would
-- replace it) is done.
injectProblemImage :: YesodExample App ()
injectProblemImage = do
  ctx <- appContext <$> getTestYesod
  _ <- liftIO $ waitForScan ctx
  injectImages [problemImage (ctxConfig ctx)]

-- | Inject images into the repository through the context
injectImages :: [Image] -> YesodExample App ()
injectImages images = do
  app <- getTestYesod
  liftIO $ atomically $ do
    let ctx = appContext app
    repo <- readTVar (ctxRepo ctx)
    let newDirs = foldl' (addImageToRepo (ctxConfig ctx)) (repoDirs repo) images
        newStats = computeRepoStats newDirs
        newExif = repoGlobalExif newDirs
        newRepo = repo { repoDirs = newDirs
                       , repoStats = newStats
                       , repoExif = newExif
                       , repoSerial = repoSerial repo + 1
                       }
    writeTVar (ctxRepo ctx) newRepo

-- | Helper to add a single image to a test folder in RepoDirs
addImageToRepo :: Config -> RepoDirs -> Image -> RepoDirs
addImageToRepo config dirs img =
  let folderName = imgParent img
      existingDir = Map.findWithDefault (createTestPicDir folderName) folderName dirs
      updatedImages = Map.insert (imgName img) img (pdImages existingDir)
      updatedDir = existingDir { pdImages = updatedImages
                               , pdTimeSort = buildTimeSort updatedImages
                               , pdStats = computeImagesStats updatedImages
                               , pdExif = buildGroupExif updatedImages
                               }
  in addDirToRepo config updatedDir dirs

-- Various test utils
shouldBeLeftWithMessage :: (Show a) => Either Text a -> Text -> Expectation
shouldBeLeftWithMessage actual expected  = case actual of
  Left msg -> unless (expected `Text.isInfixOf` msg) $
    expectationFailure ("Expected Left with message: " ++ show expected ++ ", but got: " ++ show msg)
  Right val ->
    expectationFailure ("Expected Left with message: " ++ show expected ++ ", but got Right: " ++ show val)

-- * Helpers for the repository and preview tests

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

-- | Writes a non-empty file, creating its directory if needed.
writeNonEmpty :: FilePath -> IO ()
writeNonEmpty path = do
  createDirectoryIfMissing True (takeDirectory path)
  writeFile path ("x" :: ByteString)

-- | Changes the format version of an encoded repository cache. The
-- version comes after a four-byte marker and its own length (an
-- 'Int', eight bytes here), and is a hash, so changing one of its
-- bytes gives another version.
otherCacheVersion :: BS.ByteString -> BS.ByteString
otherCacheVersion encoded =
  let (prefix, rest) = BS.splitAt 12 encoded
  in case BS.uncons rest of
       Just (b, rest') -> prefix <> BS.cons (b `xor` 1) rest'
       Nothing         -> encoded
