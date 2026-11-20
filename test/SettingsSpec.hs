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

module SettingsSpec (spec) where

import           Data.Aeson            (Value (..), encode, object, (.=))
import qualified Data.ByteString.Lazy  as BSL
import           Yesod.Default.Config2 (loadYamlSettings, useEnv)

import           Settings              (AppSettings (..))
import           TestImport

-- | Loads the test settings, overriding some values (a null value
-- unsets the setting).
--
-- The values are written to a file loaded together with the test
-- settings, as values passed directly to 'loadYamlSettings' are only
-- defaults for the files. Of the files, the first one wins.
loadSettingsWith :: [Value] -> IO AppSettings
loadSettingsWith values =
  withSystemTempFile "settings.yml" $ \path h -> do
    -- JSON is valid YAML.
    BSL.hPut h . encode $ foldr merge (object []) values
    hClose h
    loadYamlSettings [path, "config/test-settings.yml"] [] useEnv
  where merge (Object a) (Object b) = Object (a <> b)
        merge a _                   = a

spec :: Spec
spec = do
  sessionMaxAgeSpec
  secureSessionsSpec
  unixSocketSpec

unixSocketSpec :: Spec
unixSocketSpec = describe "unix-socket" $ do
  let socket = "unix-socket" .= ("/run/corydalis/socket" :: String)
  it "has no group or mode by default" $ do
    settings <- loadSettingsWith [object [socket]]
    appUnixSocketGroup settings `shouldBe` Nothing
    appUnixSocketMode settings `shouldBe` Nothing
  it "reads the group and the mode" $ do
    settings <- loadSettingsWith
      [object [ socket
              , "unix-socket-group" .= ("www-data" :: Text)
              , "unix-socket-mode" .= ("0660" :: Text)
              ]]
    appUnixSocketGroup settings `shouldBe` Just "www-data"
    appUnixSocketMode settings `shouldBe` Just 0o660
  it "refuses an unquoted mode" $
    loadSettingsWith [object [socket, "unix-socket-mode" .= (660 :: Int)]]
      `shouldThrow` anyException
  it "refuses an invalid mode" $
    loadSettingsWith [object [socket, "unix-socket-mode" .= ("0999" :: Text)]]
      `shouldThrow` anyException
  it "needs a socket for the group and mode" $ do
    loadSettingsWith [object ["unix-socket-mode" .= ("0660" :: Text)]]
      `shouldThrow` anyException
    loadSettingsWith [object ["unix-socket-group" .= ("www-data" :: Text)]]
      `shouldThrow` anyException

secureSessionsSpec :: Spec
secureSessionsSpec = describe "secure-sessions" $ do
  let load https secure socket =
        appSecureSessions <$> loadSettingsWith
          [object [ "https" .= https
                  , "secure-sessions" .= (secure :: Maybe Bool)
                  , "unix-socket" .= (socket :: Maybe String)
                  ]]
  it "defaults to the https setting" $ do
    load True Nothing Nothing `shouldReturn` True
    load False Nothing Nothing `shouldReturn` False
  it "can be enabled without https, for proxies" $
    load False (Just True) Nothing `shouldReturn` True
  it "is forced when serving https directly" $
    load True (Just False) Nothing `shouldReturn` True
  it "is not forced on a Unix socket, which ignores https" $ do
    load True (Just False) (Just "/run/socket") `shouldReturn` False
    load True Nothing (Just "/run/socket") `shouldReturn` True

sessionMaxAgeSpec :: Spec
sessionMaxAgeSpec = describe "session-max-age" $ do
  it "defaults to a week" $ do
    settings <- loadSettingsWith []
    appSessionMaxAge settings `shouldBe` 7 * 24 * 3600
  it "is given in hours" $ do
    settings <- loadSettingsWith [object ["session-max-age" .= (12 :: Int)]]
    appSessionMaxAge settings `shouldBe` 12 * 3600
  it "must be positive" $ do
    loadSettingsWith [object ["session-max-age" .= (0 :: Int)]]
      `shouldThrow` anyException
    loadSettingsWith [object ["session-max-age" .= (-1 :: Int)]]
      `shouldThrow` anyException
