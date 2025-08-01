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

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import           Control.Monad.Logger                      (liftLoc,
                                                            runLoggingT)
import           Database.Persist.Sqlite                   (createSqlitePool,
                                                            runMigrationQuiet,
                                                            runSqlPool,
                                                            sqlDatabase,
                                                            sqlPoolSize)
import           Import
import           Language.Haskell.TH.Syntax                (qLocation)
import           Network.Socket                            (Family (AF_UNIX),
                                                            SockAddr (SockAddrUnix),
                                                            SocketOption (ReuseAddr),
                                                            SocketType (Stream),
                                                            bind, listen,
                                                            maxListenQueue,
                                                            setSocketOption,
                                                            socket)
import           Network.Wai                               (Middleware)
import           Network.Wai.Handler.Warp                  (Settings,
                                                            defaultSettings,
                                                            defaultShouldDisplayException,
                                                            getPort,
                                                            runSettings,
                                                            runSettingsSocket,
                                                            setHost,
                                                            setOnException,
                                                            setPort)
import           Network.Wai.Handler.WarpTLS               (OnInsecure (..),
                                                            TLSSettings,
                                                            onInsecure, runTLS,
                                                            tlsSettings)
import           Network.Wai.Middleware.MethodOverridePost
import           Network.Wai.Middleware.RequestLogger      (Destination (Callback, Logger),
                                                            IPAddrSource (..),
                                                            OutputFormat (..),
                                                            destination,
                                                            mkRequestLogger,
                                                            outputFormat)
import           System.Directory                          (doesFileExist,
                                                            removeFile)
import           System.Log.FastLogger                     (defaultBufSize,
                                                            newStdoutLoggerSet,
                                                            toLogStr)
import           Yesod.Core.Types                          (loggerPutStr)

import           Pics                                      (initContext)

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import           Handler.Browse
import           Handler.Camera
import           Handler.Common
import           Handler.Curate
import           Handler.FlaggedImages
import           Handler.Folder
import           Handler.Home
import           Handler.Image
import           Handler.Lens
import           Handler.List
import           Handler.Reload
import           Handler.Search
import           Handler.Settings
import           Handler.Status
import           Handler.View

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
#ifdef DEVELOPMENT
        staticDevel
#else
        static
#endif
          (appStaticDir appSettings)

    -- Initialise the application context, first creating the custom app logger.
    let logfn level str = when (level >= appLogLevel appSettings) $ loggerPutStr appLogger $ str <> "\n"
    appContext <- atomically $ initContext (appConfig appSettings) logfn
    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        -- The App {..} syntax is an example of record wild cards. For more
        -- information, see:
        -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePool
        (sqlDatabase $ appDatabaseConf appSettings)
        (sqlPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    statements <- runLoggingT (runSqlPool (runMigrationQuiet migrateAll) pool) logFunc
    let prefix = toLogStr ("Migration: "::String)
    mapM_ (\m -> logfn LevelInfo $ prefix <> toLogStr m) statements

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ (defaultMiddlewaresNoLogging . methodOverridePost) appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    let apacheFormat = Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        logDestination = Logger $ loggerSet $ appLogger foundation
        (outputFormat, destination) = case appRequestLogging $ appSettings foundation of
            RequestLoggingDisabled -> (apacheFormat, Callback $ \_ -> return ())
            RequestLoggingApache   -> (apacheFormat, logDestination)
            RequestLoggingDetailed -> (Detailed True, logDestination)
    in
    mkRequestLogger def
        { outputFormat = outputFormat
        , destination = destination
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings


-- | TLS settings for the given foundation value.
appTlsSettings :: App -> TLSSettings
appTlsSettings _ =
  let s = tlsSettings "config/cert.pem" "config/cert.key"
  in s { onInsecure = DenyInsecure "This server only accepts secure HTTPS connections."
       }

-- | Helper to build the application.
getApplicationHelper :: IO (Settings, App, Application)
getApplicationHelper = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, foundation, app)

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    (wsettings, _, app) <- getApplicationHelper
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs [] useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Check if we should use a Unix socket
    case appUnixSocket settings of
        Just socketPath -> do
            --putStrLn $ "Starting application on Unix socket: " ++ socketPath
            -- Create the Unix socket
            sock <- socket AF_UNIX Stream 0
            setSocketOption sock ReuseAddr 1
            -- Remove socket file if it exists
            doesFileExist socketPath >>= flip when (removeFile socketPath)
            -- Bind to the socket path
            bind sock (SockAddrUnix socketPath)
            -- Start listening
            listen sock maxListenQueue
            -- Run the application with the socket
            runSettingsSocket (warpSettings foundation) sock app

        Nothing -> do
            -- Run the application with Warp
            let runner = if appHttps settings
                   then runTLS (appTlsSettings foundation)
                   else runSettings
            runner (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    (wsettings, foundation, app) <- getApplicationHelper
    return (getPort wsettings, foundation, app)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerFor App) a -> IO a
db = handler . runDB
