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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module TestImport
    ( module TestImport
    , module X
    ) where

import           Application                    (makeFoundation, makeLogWare)
import           ClassyPrelude                  as X hiding (Handler, delete,
                                                      deleteBy)
import           Database.Persist               as X hiding (get)
import           Database.Persist.Sql           (SqlPersistM,
                                                 runSqlPersistMPool)
import           Foundation                     as X
import           Model                          as X
import           Pics                           (Ctx, File (File), Image,
                                                 MediaType (..), initContext,
                                                 launchScanFileSystem, mkImage,
                                                 waitForScan)
import           Test.Hspec                     as X
import           Test.Hspec.Expectations.Lifted as THL (shouldSatisfy)
import           Test.Hspec.QuickCheck          as X
import           Types                          (Config (..))
import           Yesod.Auth                     as X
import           Yesod.Core.Unsafe              (fakeHandlerGetLogger)
import           Yesod.Default.Config2          (loadYamlSettings, useEnv)
import           Yesod.Test                     as X

import qualified Control.Exception              as E
import qualified Data.ByteString.Char8          as BS8
import           Data.Default
import           Data.Either
import qualified Data.Text                      as Text
import           Settings                       (AppSettings (..))
import           System.Directory               (createDirectory,
                                                 removeDirectoryRecursive)
import           System.IO.Temp
import           System.Log.FastLogger          (fromLogStr)

-- Helper functions for running tests.

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

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

-- | Builds and returns a valid context.
openTempContext :: AppSettings -> IO (FilePath, Ctx)
openTempContext settings = do
  tempDir <- setTempDir
  config <- updateConfig tempDir settings
  -- FIXME: use a proper logger? replace with one from yesod?
  let logger level msg = when (level >= appLogLevel settings) $ BS8.putStrLn . fromLogStr $ msg
  ctx <- atomically $ initContext config logger
  launchScanFileSystem ctx
  _ <- waitForScan ctx
  return (tempDir, ctx)

-- | Runs an action with a temporary context.
withContext' :: (Ctx -> IO ()) -> IO ()
withContext' action = do
  settings <- loadSettings
  bracket (openTempContext settings) (cleanupTempDir . fst) (action . snd)

-- | Spec definition for with context.
withContext :: SpecWith Ctx -> Spec
withContext = around withContext'

-- App spec definitions.

-- | Builds and returns a valid app.
openTempApp :: IO (FilePath, TestApp App)
openTempApp = do
  tempDir <- setTempDir
  settings <- loadSettings >>= updateSettings tempDir
  foundation <- makeFoundation settings
  logWare <- liftIO $ makeLogWare foundation
  return (tempDir, (foundation, logWare))

-- | Runs an action with a temporary app.
withApp' :: (TestApp App -> IO ())  -> IO ()
withApp' action = bracket openTempApp (cleanupTempDir . fst) (action . snd)

-- | Spec definition for with app.
withApp :: SpecWith (TestApp App) -> Spec
withApp = around withApp'

-- Helper test functions

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) =
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userName u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.
createUser :: Text -> YesodExample App (Entity User)
createUser ident =
    runDB $ insertEntity User
        { userName = ident
        , userPassword = Nothing
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

simpleImage :: Config -> Image
simpleImage config =
  let f = File "a.nef" 0 0 0 "/no-such-file" def
  in mkImage config "a" "b" (Just f) Nothing []
             Nothing [] [] Nothing MediaImage def

-- Various test utils
shouldBeLeftWithMessage :: (Show a) => Either Text a -> Text -> Expectation
shouldBeLeftWithMessage actual expected  = case actual of
  Left msg -> unless (expected `Text.isInfixOf` msg) $
    expectationFailure ("Expected Left with message: " ++ show expected ++ ", but got: " ++ show msg)
  Right val ->
    expectationFailure ("Expected Left with message: " ++ show expected ++ ", but got Right: " ++ show val)
