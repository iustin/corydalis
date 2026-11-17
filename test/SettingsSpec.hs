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

import           Data.Aeson            (Value, object, (.=))
import           Yesod.Default.Config2 (loadYamlSettings, useEnv)

import           Settings              (AppSettings (..))
import           TestImport

-- | Loads the test settings, overriding some values.
loadSettingsWith :: [Value] -> IO AppSettings
loadSettingsWith = flip (loadYamlSettings ["config/test-settings.yml"]) useEnv

spec :: Spec
spec = describe "session-max-age" $ do
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
