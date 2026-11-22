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

import           Settings              (AppSettings (..),
                                        insecureApprootWarning,
                                        newRenderToolLimits,
                                        newScanToolLimits)
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
  strictTransportSecuritySpec
  unixSocketSpec
  insecureApprootSpec
  storageDirsSpec
  renderToolsSpec
  scanToolsSpec

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
  it "defaults to true" $ do
    load True Nothing Nothing `shouldReturn` True
    load False Nothing Nothing `shouldReturn` True
    load False Nothing (Just "/run/socket") `shouldReturn` True
  it "can be disabled for plain HTTP" $
    load False (Just False) Nothing `shouldReturn` False
  it "can be enabled without https, for proxies" $
    load False (Just True) Nothing `shouldReturn` True
  it "is forced when serving https directly" $
    load True (Just False) Nothing `shouldReturn` True
  it "is not forced on a Unix socket, which ignores https" $ do
    load True (Just False) (Just "/run/socket") `shouldReturn` False
    load True Nothing (Just "/run/socket") `shouldReturn` True

strictTransportSecuritySpec :: Spec
strictTransportSecuritySpec = describe "strict-transport-security" $ do
  let load https hsts socket =
        appStrictTransportSecurity <$> loadSettingsWith
          [object [ "https" .= https
                  , "strict-transport-security" .= (hsts :: Maybe Bool)
                  , "unix-socket" .= (socket :: Maybe String)
                  ]]
  it "defaults to https, when serving it directly" $ do
    load True Nothing Nothing `shouldReturn` True
    load False Nothing Nothing `shouldReturn` False
  it "defaults to off on a Unix socket, which ignores https" $
    load True Nothing (Just "/run/socket") `shouldReturn` False
  it "can be set explicitly" $ do
    load True (Just False) Nothing `shouldReturn` False
    load False (Just True) Nothing `shouldReturn` True
    load True (Just True) (Just "/run/socket") `shouldReturn` True

insecureApprootSpec :: Spec
insecureApprootSpec = describe "insecureApprootWarning" $ do
  let warning root secure =
        insecureApprootWarning <$> loadSettingsWith
          [object [ "approot" .= (root :: Maybe Text)
                  , "secure-sessions" .= (secure :: Bool)
                  ]]
      warns root = warning (Just root) True >>= (`shouldSatisfy` isJust)
      quiet root secure = warning root secure >>= (`shouldBe` Nothing)
  it "warns about secure cookies on a plain HTTP root" $ do
    warns "http://photos.example.com"
    warns "http://photos.example.com:3000/"
    warns "HTTP://192.168.1.10:3000"
  it "doesn't take names starting with 127 for localhost" $ do
    warns "http://127.example.com"
    warns "http://127.0.0.1.example.com"
    warns "http://127.0.0.256"
  it "accepts an HTTPS root" $
    quiet (Just "https://photos.example.com") True
  it "accepts plain HTTP on localhost" $
    forM_ [ "http://localhost:3000", "http://localhost", "http://dev.localhost"
          , "http://127.0.0.1:3000", "http://127.1.2.3/", "http://[::1]:3000/"] $ \root ->
      quiet (Just root) True
  it "accepts plain HTTP without secure cookies" $
    quiet (Just "http://photos.example.com") False
  it "can't tell without a root" $
    quiet Nothing True

storageDirsSpec :: Spec
storageDirsSpec = describe "storage directories" $ do
  let loadWithConfig fields = loadSettingsWith [object ["config" .= object fields]]
  it "accepts absolute paths" $ do
    settings <- loadWithConfig [ "sourcedirs" .= ["/pics/raw" :: Text]
                            , "outputdirs" .= ["/pics/out" :: Text]
                            , "cachedir" .= ("/pics/cache" :: Text)
                            ]
    cfgSourceDirs (appConfig settings) `shouldBe` ["/pics/raw"]
    cfgOutputDirs (appConfig settings) `shouldBe` ["/pics/out"]
    cfgCacheDir (appConfig settings) `shouldBe` "/pics/cache"
  it "refuses relative paths" $ do
    -- The error names the setting and the path, and says why.
    let refused setting path fields =
          loadWithConfig fields `shouldThrow` \e ->
            let msg = show (e :: SomeException)
            in all (`isInfixOf` msg) [setting, path :: String, "absolute"]
    refused "sourcedirs" "raw"
      ["sourcedirs" .= ["/pics/raw", "raw" :: Text]]
    refused "outputdirs" "./out" ["outputdirs" .= ["./out" :: Text]]
    refused "cachedir" "cache" ["cachedir" .= ("cache" :: Text)]
  it "refuses a page size which isn't positive" $
    forM_ [0, -16 :: Int] $ \size ->
      -- The error names the setting and the value.
      loadWithConfig ["pagesize" .= size] `shouldThrow` \e ->
        let msg = show (e :: SomeException)
        in all (`isInfixOf` msg) ["pagesize", show size]

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

renderToolsSpec :: Spec
renderToolsSpec = describe "render tool limits" $ do
  -- The error names the setting and the offending value.
  let refused setting value =
        loadSettingsWith [object [fromString setting .= value]] `shouldThrow` \e ->
          let msg = show (e :: SomeException)
          in all (`isInfixOf` msg) [setting, show value]
  it "default to one tool per processor, for five minutes" $ do
    settings <- loadSettingsWith []
    appRenderToolSlots settings `shouldBe` defaultToolSlots
    appRenderToolTimeout settings `shouldBe` defaultRenderToolTimeout
    appRenderToolTimeout settings `shouldBe` 300 * 1000000
  it "can be configured, with the timeout in seconds" $ do
    settings <- loadSettingsWith [object [ "render-tool-slots" .= (2 :: Int)
                                         , "render-tool-timeout" .= (60 :: Int)
                                         ]]
    appRenderToolSlots settings `shouldBe` 2
    appRenderToolTimeout settings `shouldBe` 60 * 1000000
    limits <- atomically $ newRenderToolLimits settings
    tlTimeout limits `shouldBe` 60 * 1000000
  it "must be positive" $ do
    refused "render-tool-slots" (0 :: Int)
    refused "render-tool-slots" (-1 :: Int)
    refused "render-tool-timeout" (0 :: Int)
    refused "render-tool-timeout" (-5 :: Int)
  it "refuses a timeout which doesn't fit in microseconds" $
    refused "render-tool-timeout" (maxBound :: Int)

scanToolsSpec :: Spec
scanToolsSpec = describe "scan tool limits" $ do
  -- The error names the setting and the offending value.
  let refused setting value =
        loadSettingsWith [object [fromString setting .= value]] `shouldThrow` \e ->
          let msg = show (e :: SomeException)
          in all (`isInfixOf` msg) [setting, show value]
  it "default to one tool per processor, for ten minutes" $ do
    settings <- loadSettingsWith []
    appScanToolSlots settings `shouldBe` defaultToolSlots
    appScanToolTimeout settings `shouldBe` defaultScanToolTimeout
    appScanToolTimeout settings `shouldBe` 600 * 1000000
  it "can be configured, independently of the preview ones" $ do
    settings <- loadSettingsWith [object [ "scan-tool-slots" .= (3 :: Int)
                                         , "scan-tool-timeout" .= (90 :: Int)
                                         ]]
    appScanToolSlots settings `shouldBe` 3
    appScanToolTimeout settings `shouldBe` 90 * 1000000
    appRenderToolSlots settings `shouldBe` defaultToolSlots
    appRenderToolTimeout settings `shouldBe` defaultRenderToolTimeout
    limits <- atomically $ newScanToolLimits settings
    tlTimeout limits `shouldBe` 90 * 1000000
    takeFreeSlots limits `shouldReturn` 3
  it "must be positive" $ do
    refused "scan-tool-slots" (0 :: Int)
    refused "scan-tool-timeout" (-5 :: Int)
  it "refuses a timeout which doesn't fit in microseconds" $
    refused "scan-tool-timeout" (maxBound :: Int)
