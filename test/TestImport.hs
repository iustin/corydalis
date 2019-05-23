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

module TestImport
    ( module TestImport
    , module X
    ) where

import           Application                    (makeFoundation, makeLogWare)
import           ClassyPrelude                  as X hiding (Handler, delete,
                                                      deleteBy)
import           Database.Persist               as X hiding (get)
import           Database.Persist.Sql           (SqlBackend, SqlPersistM,
                                                 connEscapeName, rawExecute,
                                                 rawSql, runSqlPersistMPool,
                                                 unSingle)
import           Foundation                     as X
import           Model                          as X
import           Pics                           (Ctx, initContext,
                                                 launchScanFileSystem,
                                                 waitForScan)
import           Test.Hspec                     as X hiding (shouldSatisfy)
import           Test.Hspec.Expectations.Lifted
import           Types                          (Config (..))
import           Yesod.Auth                     as X
import           Yesod.Core.Unsafe              (fakeHandlerGetLogger)
import           Yesod.Default.Config2          (loadYamlSettings, useEnv)
import           Yesod.Test                     as X

import qualified Control.Exception              as E
import           Control.Monad.Logger           (runLoggingT)
import qualified Data.ByteString.Char8          as BS8
import           Data.Either
import qualified Data.Text                      as T
import           Database.Persist.Sqlite        (SqliteConf (..), createSqlPool,
                                                 sqlDatabase, wrapConnection)
import qualified Database.Sqlite                as Sqlite
import           Settings                       (AppSettings (..),
                                                 appDatabaseConf)
import           System.Directory               (createDirectory,
                                                 removeDirectoryRecursive)
import           System.IO.Temp
import           System.Log.FastLogger          (fromLogStr)
import           Yesod.Core                     (messageLoggerSource)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    pool <- fmap appConnPool getTestYesod
    liftIO $ runSqlPersistMPool query pool

runHandler :: Handler a -> YesodExample App a
runHandler handler = do
    app <- getTestYesod
    fakeHandlerGetLogger appLogger app handler

loadSettings :: IO AppSettings
loadSettings =
  loadYamlSettings
    ["config/test-settings.yml"]
    []
    useEnv

setTempDir :: AppSettings -> IO (FilePath, (AppSettings, Ctx))
setTempDir settings = do
  rootTempDir <- getCanonicalTemporaryDirectory
  tempDir <- createTempDirectory rootTempDir "corydalis-test"
  let inTemp = (tempDir </>)
      dbDir = inTemp "db"
      dbPath = dbDir </> "test-db.sqlite3"
      rawDir = inTemp "raw"
      jpgDir = inTemp "jpg"
  createDirectory dbDir
  createDirectory rawDir
  createDirectory jpgDir
  let config = appConfig settings
      config' = config { cfgCacheDir = tempDir </> "cache"
                       , cfgSourceDirs = [rawDir]
                       , cfgOutputDirs = [jpgDir]
                       }
      db = SqliteConf (T.pack dbPath) 10
      settings' = settings { appConfig = config', appDatabaseConf = db }
      -- FIXME: use a proper logger? replace with one from yesod?
      logger = BS8.putStrLn . fromLogStr
  ctx <- atomically $ initContext config' logger
  launchScanFileSystem ctx
  _ <- waitForScan ctx
  return (tempDir, (settings', ctx))

{-# ANN ignoringIOErrors ("HLint: ignore Evaluate"::String) #-}
-- | Adapted from temporary's code.
ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe =
  ioe `E.catch` (\e -> const (return ()) (e :: IOException))

cleanupTempDir :: (FilePath, (a, b)) -> IO ()
cleanupTempDir  = ignoringIOErrors . removeDirectoryRecursive . fst

withTempContext :: (Ctx -> IO ()) -> IO ()
withTempContext action = do
  settings <- loadSettings
  bracket (setTempDir settings) cleanupTempDir (action . snd . snd)

openTempApp :: IO (FilePath, TestApp App)
openTempApp = do
  (tempDir, (settings, _)) <- loadSettings >>= setTempDir
  foundation <- makeFoundation settings
  wipeDB foundation
  logWare <- liftIO $ makeLogWare foundation
  return (tempDir, (foundation, logWare))

withApp :: SpecWith (TestApp App) -> Spec
withApp =
  around (\action -> bracket openTempApp cleanupTempDir (action . snd))

withContext :: SpecWith Ctx -> Spec
withContext = around withTempContext

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = do
    -- In order to wipe the database, we need to temporarily disable foreign key checks.
    -- Unfortunately, disabling FK checks in a transaction is a noop in SQLite.
    -- Normal Persistent functions will wrap your SQL in a transaction,
    -- so we create a raw SQLite connection to disable foreign keys.
    -- Foreign key checks are per-connection, so this won't effect queries outside this function.

    -- Aside: SQLite by default *does not enable foreign key checks*
    -- (disabling foreign keys is only necessary for those who specifically enable them).
    let settings = appSettings app
    sqliteConn <- rawConnection (sqlDatabase $ appDatabaseConf settings)
    disableForeignKeys sqliteConn

    let logFunc = messageLoggerSource app (appLogger app)
    pool <- runLoggingT (createSqlPool (wrapConnection sqliteConn) 1) logFunc

    flip runSqlPersistMPool pool $ do
        tables <- getTables
        sqlBackend <- ask
        let queries = map (\t -> "DELETE FROM " ++ connEscapeName sqlBackend (DBName t)) tables
        forM_ queries (\q -> rawExecute q [])

rawConnection :: Text -> IO Sqlite.Connection
rawConnection = Sqlite.open

disableForeignKeys :: Sqlite.Connection -> IO ()
disableForeignKeys conn = Sqlite.prepare conn "PRAGMA foreign_keys = OFF;" >>= void . Sqlite.step

getTables :: MonadIO m => ReaderT SqlBackend m [Text]
getTables =
    fmap unSingle <$>
        rawSql "SELECT name FROM sqlite_master WHERE type = 'table';" []

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
    redir `shouldSatisfy` isRight
