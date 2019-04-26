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
  , widgetFile
  , combineStylesheets
  , combineScripts
  ) where

import           ClassyPrelude.Yesod
import           Data.Aeson                 (parseJSON, withObject, (.!=), (.:),
                                             (.:?))
import           Database.Persist.Sqlite    (SqliteConf)
import           Language.Haskell.TH.Syntax (Exp, Name, Q)
import           Network.Wai.Handler.Warp   (HostPreference)
import           Yesod.Default.Util

import           Types

-- | Runtime settings to configure this application. These settings can be
-- loaded from various sources: defaults, environment variables, config files,
-- theoretically even a database.
data AppSettings = AppSettings
    { appStaticDir              :: String
    -- ^ Directory from which to serve static files.
    , appDatabaseConf           :: SqliteConf
    -- ^ Configuration settings for accessing the database.
    , appRoot                   :: Maybe Text
    -- ^ Base for all generated URLs. If @Nothing@, determined
    -- from the request headers.
    , appHost                   :: HostPreference
    -- ^ Host/interface the server should bind to.
    , appPort                   :: Int
    -- ^ Port to listen on
    , appHttps                  :: Bool
    -- ^ Whether to run HTTPS or not on the configured host/port. This
    -- is mostly available for reverse proxies, where adding TLS
    -- doesn't much any additional security. For direct access
    -- (without a proxy), this is always recommended.
    , appSecureSessions         :: Bool
    -- ^ Enable secure cookies, and set a Strict-Transport-Security
    -- header on the connections. When https is set, this is
    -- overriden, and when https is unset, this can be helpful in case
    -- of reverse proxying.
    , appIpFromHeader           :: Bool
    -- ^ Get the IP address from the header when logging. Useful when sitting
    -- behind a reverse proxy.

    , appLoginMessage           :: Maybe Text
    -- ^ Extra message to show on login page.
    , appHomeMessage            :: Maybe Text
    -- ^ Extra message to show on the main page. Useful for example
    -- for demo or public sites.

    , appDetailedRequestLogging :: Bool
    -- ^ Use detailed request logging system
    , appShouldLogAll           :: Bool
    -- ^ Should all log messages be displayed?

    , appConfig                 :: Config
    -- ^ Picture-related configuration
    }

isDevel :: Bool
isDevel =
#ifdef DEVELOPMENT
  True
#else
  False
#endif

instance FromJSON AppSettings where
    parseJSON = withObject "AppSettings" $ \o -> do
        let defaultDev = isDevel
        appStaticDir              <- o .:  "static-dir"
        appDatabaseConf           <- o .:  "database"
        appRoot                   <- o .:? "approot"
        appHost                   <- fromString <$> o .: "host"
        appPort                   <- o .:  "port"
        appHttps                  <- o .:  "https"
        appSecureSessions         <- o .:  "secure-sessions"
        appIpFromHeader           <- o .:  "ip-from-header"
        appLoginMessage           <- o .:? "login-msg"
        appHomeMessage            <- o .:? "home-msg"

        appDetailedRequestLogging <- o .:? "detailed-logging" .!= defaultDev
        appShouldLogAll           <- o .:? "should-log-all"   .!= defaultDev

        appConfig                 <- o .:  "config"

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
