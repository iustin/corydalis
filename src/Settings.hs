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
  , widgetFile
  , combineStylesheets
  , combineScripts
  ) where

import           ClassyPrelude.Yesod
import           Data.Aeson                 (withObject, (.!=), (.:?))
import           Data.Aeson.Types           (Parser, parseFail)
import           Database.Persist.Sqlite    (SqliteConf)
import           Language.Haskell.TH.Syntax (Exp, Name, Q)
import           Network.Wai.Handler.Warp   (HostPreference)
import           Yesod.Default.Util

import           Types

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
    -- ^ Enable secure cookies. When https is set, this is forced to true,
    -- and when https is unset, this should be enabled when Corydalis is
    -- reverse-proxied.
    , appStrictTransportSecurity :: Bool
    -- ^ Emit a Strict-Transport-Security header in the response. This
    -- defaults to the https parameter; in case of plain http, it can be
    -- useful when reverse proxied. However, it's preferable to configure
    -- this in the reverse proxy itself, since that's more commonly done
    -- on the entire (apex) domain (e.g. on example.com), rather than in
    -- individual apps like Corydalis, hosted on a leaf name (e.g.
    -- corydalis.example.com).
    , appIpFromHeader   :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

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
        appHost           <-  fromString <$> o .: "host"
        appPort           <- o .:  "port"
        appHttps          <- o .:  "https"
        appSecureSessions <- o .:  "secure-sessions"
        appStrictTransportSecurity <- o .:? "strict-transport-security" .!= appHttps
        appIpFromHeader   <- o .:  "ip-from-header"
        appLoginMessage   <- o .:? "login-msg"
        appHomeMessage    <- o .:? "home-msg"
        appRequestLogging <- o .:? "request-logging" .!= defaultRequestLogging
        appShouldLogAll   <- o .:? "should-log-all" .!= defaultDev
        logLevel          <- o .:? "log-level" .!= "info"
        appLogLevel       <- parseLogLevel logLevel

        appConfig         <- o .:  "config"

        return AppSettings {..}

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
