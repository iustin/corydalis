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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings
  ( AppSettings(..)
  , RequestLogging(..)
  , insecureApprootWarning
  , newRenderToolLimits
  , newScanToolLimits
  , widgetFile
  , combineStylesheets
  , combineScripts
  ) where

import           ClassyPrelude.Yesod
import           Data.Aeson                 (withObject, (.!=), (.:?))
import           Data.Char                  (isDigit)
import           Data.Aeson.Types           (Object, Parser, parseFail)
import           Data.Time                  (NominalDiffTime)
import           Database.Persist.Sqlite    (SqliteConf)
import           Language.Haskell.TH.Syntax (Exp, Name, Q)
import           Network.Wai.Handler.Warp   (HostPreference)
import           System.Posix.Types         (FileMode)
import           Yesod.Default.Util

import           Types
import           Utils.Socket               (parseSocketMode)

-- | Denotes the type of request logging to be performed.
data RequestLogging = RequestLoggingDisabled
                    | RequestLoggingApache
                    | RequestLoggingDetailed
                    deriving (Eq, Show)

instance FromJSON RequestLogging where
    parseJSON "disabled" = return RequestLoggingDisabled
    parseJSON "apache"   = return RequestLoggingApache
    parseJSON "detailed" = return RequestLoggingDetailed
    parseJSON v          = parseFail $ "Invalid request logging type: " ++ show v

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir      :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf   :: SqliteConf
    -- ^ Configuration settings for accessing the database.
    , appRoot           :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appUnixSocket     :: Maybe String
    -- ^ The path to use as a Unix socket, using plain HTTP (no
    -- encryption), instead of the host and port settings below. This
    -- overrides those settings, and also ignores the https setting. The
    -- secure sessions and HSTS settings should be configured as needed.
    -- Useful mainly for reverse proxied deployments.
    , appUnixSocketGroup :: Maybe Text
    -- ^ The group to give the Unix socket, e.g. one shared with the
    -- reverse proxy; the user running Corydalis must be a member of
    -- it. If not set, the socket gets the process' group.
    , appUnixSocketMode :: Maybe FileMode
    -- ^ The permissions of the Unix socket; a proxy needs write
    -- permission to connect. If not set, they come from the umask.
    , appHost           :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort           :: Int
    -- ^ Port to listen on
    , appHttps          :: Bool
    -- ^ Whether to run HTTPS or not on the configured host/port. This
    -- is mostly available for reverse proxies, where adding TLS
    -- doesn't much any additional security. For direct access
    -- (without a proxy), this is always recommended.
    , appSecureSessions :: Bool
    -- ^ Whether the session cookies are secure (only sent over HTTPS).
    -- The secure-sessions setting defaults to true, and when serving
    -- HTTPS directly (https set, and no Unix socket), cookies are
    -- always secure; only plain HTTP setups (other than on localhost,
    -- which browsers treat as secure) need to disable it.
    , appStrictTransportSecurity :: Bool
    -- ^ Emit a Strict-Transport-Security header in the response. This
    -- defaults to the https parameter, except with a Unix socket (which
    -- ignores https), where it defaults to false; in case of plain http, it can be
    -- useful when reverse proxied. However, it's preferable to configure
    -- this in the reverse proxy itself, since that's more commonly done
    -- on the entire (apex) domain (e.g. on example.com), rather than in
    -- individual apps like Corydalis, hosted on a leaf name (e.g.
    -- corydalis.example.com).
    , appIpFromHeader   :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.
    , appSessionMaxAge  :: NominalDiffTime
    -- ^ How long a login is valid, regardless of activity; after this,
    -- the user has to log in again.

    , appRenderToolSlots :: Int
    -- ^ How many external tools (ImageMagick, ffmpeg and exiftool) may
    -- run at once to build previews; further previews wait.
    , appRenderToolTimeout :: Int
    -- ^ How long such a tool may run, in microseconds, before it is
    -- stopped, and the preview fails.
    , appScanToolSlots :: Int
    -- ^ How many exiftool runs the scan may have at once; further runs
    -- wait.
    , appScanToolTimeout :: Int
    -- ^ How long such a run may take, in microseconds, before it is
    -- stopped, and the exif data of its files fails.

    , appLoginMessage   :: Maybe Text
    -- ^ Extra message to show on login page.
    , appHomeMessage    :: Maybe Text
    -- ^ Extra message to show on the main page. Useful for example
    -- for demo or public sites.

    , appRequestLogging :: RequestLogging
    -- ^ Use detailed request logging system
    , appShouldLogAll   :: Bool
    -- ^ Should all log messages be displayed?
    , appLogLevel       :: LogLevel
    -- ^ Log level for the application
    , appConfig         :: Config
    -- ^ Picture-related configuration
    }

isDevel :: Bool
isDevel =
#ifdef DEVELOPMENT
  True
#else
  False
#endif

-- | The default maximum age of a login session, in hours (a week).
defaultSessionMaxAge :: Int
defaultSessionMaxAge = 7 * 24

parseLogLevel :: String -> Parser LogLevel
parseLogLevel "debug" = return LevelDebug
parseLogLevel "info"  = return LevelInfo
parseLogLevel "warn"  = return LevelWarn
parseLogLevel "error" = return LevelError
parseLogLevel v       = parseFail $ "Invalid log level (use debug, info, warn, error): " ++ v

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev            = isDevel
            defaultRequestLogging = if defaultDev then RequestLoggingDetailed else RequestLoggingApache
        appStaticDir      <- o .:  "static-dir"
        appDatabaseConf   <- o .:  "database"
        appRoot           <- o .:? "approot"
        appUnixSocket     <- o .:? "unix-socket"
        appUnixSocketGroup <- o .:? "unix-socket-group"
        appUnixSocketMode <- o .:? "unix-socket-mode" >>=
                             traverse parseSocketModeValue
        when (isNothing appUnixSocket &&
              (isJust appUnixSocketGroup || isJust appUnixSocketMode)) $
          parseFail "unix-socket-group and unix-socket-mode need unix-socket"
        appHost           <-  fromString <$> o .: "host"
        appPort           <- o .:  "port"
        appHttps          <- o .:  "https"
        -- Secure by default: everything but local development should
        -- be served over HTTPS, if only by a reverse proxy.
        secureSessions    <- o .:? "secure-sessions" .!= True
        -- Serving HTTPS directly (the Unix socket ignores the https
        -- setting), the cookies must not be sent in clear.
        let appSecureSessions =
              secureSessions || (appHttps && isNothing appUnixSocket)
        -- By default, only when serving HTTPS directly (the condition
        -- that forces the secure sessions on, above): behind a proxy, it
        -- is the proxy's choice (and a header from here could override
        -- its own).
        appStrictTransportSecurity <- o .:? "strict-transport-security" .!=
                                        (appHttps && isNothing appUnixSocket)
        appIpFromHeader   <- o .:  "ip-from-header"
        sessionMaxAge     <- o .:? "session-max-age" .!= defaultSessionMaxAge
        when (sessionMaxAge <= 0) $
          parseFail $ "Invalid session-max-age (must be positive): " ++
            show sessionMaxAge
        let appSessionMaxAge = fromIntegral sessionMaxAge * 3600
        (appRenderToolSlots, appRenderToolTimeout) <-
          parseToolLimits o "render" defaultRenderToolTimeout
        (appScanToolSlots, appScanToolTimeout) <-
          parseToolLimits o "scan" defaultScanToolTimeout
        appLoginMessage   <- o .:? "login-msg"
        appHomeMessage    <- o .:? "home-msg"
        appRequestLogging <- o .:? "request-logging" .!= defaultRequestLogging
        appShouldLogAll   <- o .:? "should-log-all" .!= defaultDev
        logLevel          <- o .:? "log-level" .!= "info"
        appLogLevel       <- parseLogLevel logLevel

        appConfig         <- o .:  "config"

        return AppSettings {..}

-- | Parses the limits of a kind of tools: how many may run at once
-- (@KIND-tool-slots@), and for how long (@KIND-tool-timeout@, in
-- seconds), returned in microseconds.
parseToolLimits :: Object
                -> String  -- ^ The kind of tools, as in the setting names.
                -> Int     -- ^ The default time limit, in microseconds.
                -> Parser (Int, Int)
parseToolLimits o kind defaultTimeout = do
  let slotsKey = kind ++ "-tool-slots"
      timeoutKey = kind ++ "-tool-timeout"
  slots <- o .:? fromString slotsKey .!= defaultToolSlots
  when (slots <= 0) $
    parseFail $ "Invalid " ++ slotsKey ++ " (must be positive): " ++ show slots
  toolTimeout <- o .:? fromString timeoutKey .!= (defaultTimeout `div` 1000000)
  -- The limit is kept in microseconds, which must fit in an Int.
  let maxToolTimeout = maxBound `div` 1000000 :: Int
  when (toolTimeout <= 0 || toolTimeout > maxToolTimeout) $
    parseFail $ "Invalid " ++ timeoutKey ++ " (must be a positive " ++
      "number of seconds, at most " ++ show maxToolTimeout ++ "): " ++
      show toolTimeout
  return (slots, toolTimeout * 1000000)

-- | Creates the limits for the preview tools, as configured.
newRenderToolLimits :: AppSettings -> STM ToolLimits
newRenderToolLimits settings =
  newToolLimits (appRenderToolSlots settings) (appRenderToolTimeout settings)

-- | Creates the limits for the scan's exiftool runs, reading the
-- metadata, as configured.
newScanToolLimits :: AppSettings -> STM ToolLimits
newScanToolLimits settings =
  newToolLimits (appScanToolSlots settings) (appScanToolTimeout settings)

-- | Returns a warning if the session cookies are secure, but the
-- configured application root is a plain HTTP URL, other than on
-- localhost.
--
-- Browsers don't keep secure cookies received over plain HTTP (except
-- from localhost, which they treat as secure), so logging in would
-- silently fail, going back to the login page.
insecureApprootWarning :: AppSettings -> Maybe Text
insecureApprootWarning settings = do
  guard $ appSecureSessions settings
  root <- appRoot settings
  rest <- stripPrefix "http://" (toLower root)
  guard . not . isLocalHost $ urlHost rest
  return $ "The approot (" <> root <> ") is a plain HTTP URL, but the " <>
    "session cookies are secure (secure-sessions), so browsers won't " <>
    "keep them, and logging in won't work; set secure-sessions to false " <>
    "if Corydalis is served over plain HTTP"
  where
    -- The host part of a URL, without the scheme (an IPv6 address
    -- keeps its brackets).
    urlHost :: Text -> Text
    urlHost rest
      | "[" `isPrefixOf` rest = takeWhile (/= ']') rest <> "]"
      | otherwise = takeWhile (\c -> c /= ':' && c /= '/') rest
    isLocalHost :: Text -> Bool
    isLocalHost host =
      host == "localhost" || ".localhost" `isSuffixOf` host ||
      isIPv4Loopback host || host == "[::1]"
    -- An IPv4 loopback address (127.0.0.0/8), written as four numbers,
    -- and not just any name starting with "127." (e.g. 127.example.com).
    isIPv4Loopback :: Text -> Bool
    isIPv4Loopback host =
      case map octet (splitElem '.' host) of
        [Just 127, Just _, Just _, Just _] -> True
        _ -> False
    octet :: Text -> Maybe Int
    octet t
      | not (null t) && length t <= 3 && all isDigit t =
          readMay t >>= \n -> if n <= 255 then Just n else Nothing
      | otherwise = Nothing

-- | Parses the Unix socket mode, which must be a (quoted) string: YAML
-- doesn't agree on how to read unquoted numbers with a leading zero
-- (as octal, or as decimal).
parseSocketModeValue :: Value -> Parser FileMode
parseSocketModeValue (String txt) =
  either (parseFail . unpack) return $ parseSocketMode txt
parseSocketModeValue _ =
  parseFail "unix-socket-mode must be a quoted octal string, e.g. \"0660\""

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
--
-- For more information on modifying behavior, see:
--
-- https://github.com/yesodweb/yesod/wiki/Overriding-widgetFile
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def

-- | How static files should be combined.
combineSettings :: CombineSettings
combineSettings = def

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile =
#ifdef DEVELOPMENT
  widgetFileReload
#else
  widgetFileNoReload
#endif
    widgetFileSettings

-- The following two functions can be used to combine multiple CSS or JS files
-- at compile time to decrease the number of http requests.
-- Sample usage (inside a Widget):
--
-- > $(combineStylesheets 'StaticR [style1_css, style2_css])

combineStylesheets :: Name -> [Route Static] -> Q Exp
combineStylesheets =
  combineStylesheets' isDevel combineSettings

combineScripts :: Name -> [Route Static] -> Q Exp
combineScripts =
  combineScripts' isDevel combineSettings
