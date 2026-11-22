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
    , requestLoggerSettings
    , makeLogFn
    , redactPassword
    , appMiddlewares
    , defaultCacheControl
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import           Control.Monad.Logger                      (liftLoc,
                                                            runLoggingT)
import qualified Data.ByteString                           as BS
import           Database.Persist.Sqlite                   (createSqlitePool,
                                                            runMigrationQuiet,
                                                            runSqlPool,
                                                            sqlDatabase,
                                                            sqlPoolSize)
import           Import
import           Language.Haskell.TH.Syntax                (qLocation)
import           Network.Wai                               (Middleware,
                                                            mapResponseHeaders,
                                                            pathInfo)
import qualified Network.Wai                               as Wai
import           Network.Wai.Handler.Warp                  (Settings,
                                                            defaultSettings,
                                                            defaultShouldDisplayException,
                                                            runSettings,
                                                            runSettingsSocket,
                                                            setHost,
                                                            setOnException,
                                                            setPort)
import           Network.Wai.Handler.WarpTLS               (OnInsecure (..),
                                                            TLSSettings,
                                                            onInsecure, runTLS,
                                                            tlsSettings)
import           Network.Wai.Middleware.AddHeaders         (addHeaders)
import           Network.Wai.Parse                          (Param)
import           Network.Wai.Middleware.RequestLogger      (DetailedSettings (..),
                                                            Destination (Callback, Handle, Logger),
                                                            IPAddrSource (..),
                                                            OutputFormat (..),
                                                            RequestLoggerSettings,
                                                            defaultDetailedSettings,
                                                            destination,
                                                            mkRequestLogger,
                                                            outputFormat)
import           System.Log.FastLogger                     (LogStr,
                                                            defaultBufSize,
                                                            flushLogStr,
                                                            fromLogStr,
                                                            newStdoutLoggerSet,
                                                            pushLogStr,
                                                            toLogStr)
import           Yesod.Core.Types                          (Logger,
                                                            loggerPutStr)
import           Utils.Logging                             (sanitizeLogLines,
                                                            sanitizeLogStr)

import           Cache                                     (cacheTempDir,
                                                            resetCacheTempDir)
import           Pics                                      (checkMagickPolicy,
                                                            initContext)

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
import           System.FilePath                           (takeDirectory)
import           Utils.Permissions                         (databaseDirectory,
                                                            databaseFileDirectory,
                                                            privateDirMessage,
                                                            protectPrivateDir)
import           Utils.Socket                              (listenUnixSocket)
import           Utils.Storage                             (checkPrivateDirsOutsideStorage)

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
    let logfn = makeLogFn (appLogLevel appSettings) appLogger
        config = appConfig appSettings

    -- Secure session cookies can't work on a plain HTTP site.
    mapM_ (logfn LevelWarn . toLogStr) $ insecureApprootWarning appSettings

    -- Restrict the directories holding private files, before anything
    -- creates them: the session key (created by the session backend) and
    -- the database (created below).
    forM_ (takeDirectory clientSessionKeyFile :
           maybeToList (databaseDirectory . sqlDatabase $ appDatabaseConf appSettings)) $
      \dir -> protectPrivateDir dir >>=
        mapM_ (logfn LevelWarn . toLogStr) . privateDirMessage dir

    -- Refuse to start if the private files are within the picture
    -- directories, where they would be listed, and possibly served, or
    -- within the cache directory, where they could be deleted.
    checkPrivateDirsOutsideStorage config
      (takeDirectory clientSessionKeyFile :
       maybeToList (databaseFileDirectory . sqlDatabase $ appDatabaseConf appSettings))
      >>= either (throwIO . userError . unpack) return

    -- Refuse to start if a configured ImageMagick policy isn't in use,
    -- as previews would then be built without it.
    case cfgMagickPolicyDir config of
      Nothing -> logfn LevelWarn
        "No ImageMagick policy configured (magickpolicydir), see the installation documentation"
      Just dir -> checkMagickPolicy config >>=
        either (throwIO . userError . unpack)
               (const . logfn LevelInfo . toLogStr $ "Using the ImageMagick policy in " <> dir)

    -- Empty the directory of the cache files being written, before
    -- anything writes one: what's there was left behind by a crash. A
    -- failure isn't fatal: the pages and the existing cache files can
    -- still be served, and building new previews then fails with the
    -- reason, if it fails too.
    tryIO (resetCacheTempDir config) >>= either
      (\e -> logfn LevelWarn . toLogStr $ "Cannot empty the directory " <>
             cacheTempDir config <> " (temporary cache files): " <> show e)
      return

    appContext <- atomically $ do
      tools <- newPreviewToolLimits appSettings
      scanTools <- newScanToolLimits appSettings
      initContext config logfn tools scanTools
    -- The user to check unknown users' passwords against, at login.
    appDummyLoginUser <- makeDummyLoginUser

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
    return $ logWare $ appMiddlewares appPlain

-- | The middlewares applied on top of the plain application, except for
-- logging.
--
-- The security headers are added at this level, rather than in the Yesod
-- middleware, so that they cover all responses, including errors. The
-- nosniff header prevents browsers from guessing a different type than
-- the one we send, e.g. executing a file as a script. The referrer
-- policy keeps our URLs (which contain folder and image names, and
-- search terms) from being sent to other sites, e.g. when following a
-- link.
appMiddlewares :: Middleware
appMiddlewares =
  addHeaders [ ("Content-Security-Policy", contentSecurityPolicy)
             , ("X-Content-Type-Options", "nosniff")
             , ("Referrer-Policy", "same-origin")
             ] .
  defaultCacheControl .
  defaultMiddlewaresNoLogging

-- | Ensures that responses without an explicit caching policy are
-- neither stored by shared caches nor reused without revalidation.
--
-- Successful static asset responses (under @/static/@) are left to
-- yesod-static, and responses that already set their own @Cache-Control@
-- header (such as 'getFaviconR' and Yesod's redirects) keep it. All
-- other responses (including dynamic pages, JSON responses, served
-- images and movies, auth pages, and errors) receive
-- @Cache-Control: private, no-cache@.
defaultCacheControl :: Middleware
defaultCacheControl app req sendResp = app req $ \res ->
  if isCacheableStatic req res
    then sendResp res
    else sendResp $ mapResponseHeaders addCacheControl res
  where
    isCacheableStatic r res =
      case pathInfo r of
        "static":_ -> let code = statusCode (Wai.responseStatus res)
                      in (code >= 200 && code < 300) || code == 304
        _          -> False
    addCacheControl headers
      | any ((== hCacheControl) . fst) headers = headers
      | otherwise = (hCacheControl, "private, no-cache") : headers

-- | The Content-Security-Policy sent with all responses.
--
-- All scripts must come from our own static files, so that injected
-- markup cannot run code; see the test ensuring no page has inline
-- scripts.
contentSecurityPolicy :: ByteString
contentSecurityPolicy = intercalate "; "
  [ "default-src 'self'"
  , "script-src 'self'"
    -- Plotly creates style elements, and some templates use style
    -- attributes.
  , "style-src 'self' 'unsafe-inline'"
    -- Bootstrap's CSS uses data: icons. (Plotly's image export would also
    -- need blob:, but it is disabled in all charts.)
  , "img-src 'self' data:"
  , "object-src 'none'"
  , "base-uri 'self'"
  , "form-action 'self'"
    -- Disallow framing, to prevent clickjacking.
  , "frame-ancestors 'none'"
  ]

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger $ requestLoggerSettings
      (appIpFromHeader settings) (appRequestLogging settings)
      (Logger $ loggerSet $ appLogger foundation)
  where settings = appSettings foundation

-- | The settings of the request logger, given whether to take the client
-- address from the headers, the kind of logging, and where to log.
requestLoggerSettings :: Bool -> RequestLogging -> Destination
                      -> RequestLoggerSettings
requestLoggerSettings ipFromHeader logging logDestination =
    let apacheFormat = Apache
                        (if ipFromHeader
                            then FromFallback
                            else FromSocket)
        -- wai-logger escapes the fields of the Apache lines taken from
        -- the request, as Apache httpd does, so these lines are logged as
        -- they are; escaping them again would escape the backslashes of
        -- the escapes. The detailed format writes several lines per
        -- request, which are escaped one by one (see "Utils.Logging").
        (outputFormat, destination) = case logging of
            RequestLoggingDisabled -> (apacheFormat, Callback $ \_ -> return ())
            RequestLoggingApache   -> (apacheFormat, logDestination)
            RequestLoggingDetailed ->
              (DetailedWithSettings detailed,
               sanitizedDestination sanitizeLogLines logDestination)
        -- The detailed format logs the request parameters, so the
        -- passwords (from the login form) are hidden; with a filter for
        -- the parameters, it also doesn't log the raw request bodies.
        detailed = defaultDetailedSettings { mModifyParams = Just redactPassword }
    in
    def { outputFormat = outputFormat
        , destination = destination
        }


-- | A destination for log lines, which escapes them first (with the
-- given function).
--
-- The result is always a callback, which (unlike the other kinds of
-- destination) the request logger doesn't flush; so for a logger set or
-- a handle, this flushes them after each line, as the request logger
-- does for these. A wrapped callback has nothing to flush.
sanitizedDestination :: (LogStr -> LogStr) -> Destination -> Destination
sanitizedDestination sanitize dest = Callback $ \str -> case dest of
  Logger set  -> pushLogStr set (sanitize str) >> flushLogStr set
  Handle h    -> BS.hPut h (fromLogStr (sanitize str)) >> hFlush h
  Callback cb -> cb (sanitize str)

-- | The application's own log function: logs messages of at least the
-- given level, escaped (see "Utils.Logging"), one per line.
makeLogFn :: LogLevel -> Logger -> LogFn
makeLogFn minLevel logger level str =
  when (level >= minLevel) . loggerPutStr logger $ sanitizeLogStr str <> "\n"

-- | Hides the value of the password parameter (of the login form).
redactPassword :: Param -> Maybe Param
redactPassword (name, value)
  | name == "password" = Just (name, "(hidden)")
  | otherwise          = Just (name, value)

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
            sock <- listenUnixSocket socketPath
                      (appUnixSocketGroup settings)
                      (appUnixSocketMode settings)
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
-- | Returns the full Warp settings, and not just the port, so that the
-- REPL server listens on the configured host as well: it runs with dummy
-- authentication enabled, so it must not be reachable from more places
-- than configured.
getApplicationRepl :: IO (Settings, App, Application)
getApplicationRepl = getApplicationHelper

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
