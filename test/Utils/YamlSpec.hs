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

module Utils.YamlSpec (spec) where

import qualified Data.ByteString as BS
import           Data.Either     (isLeft)

import           TestImport
import           Utils.Yaml

spec :: Spec
spec = describe "loadYaml" $ do
  it "loads an event, recording its source" $
    withSystemTempDirectory "corydalis-yaml" $ \dir -> do
      let path = dir </> "corydalis.yaml"
      BS.writeFile path "name: Trip\npeople: [Alice]\n"
      loadYaml path `shouldReturn` Right GenericEvent
        { eventName = "Trip"
        , eventPeople = ["Alice"]
        , eventSource = EventExplicit (Just path)
        }
  it "returns the error for an invalid file" $
    withSystemTempDirectory "corydalis-yaml" $ \dir -> do
      let path = dir </> "corydalis.yaml"
      BS.writeFile path "name: [unclosed\n"
      (loadYaml path :: IO (Either String Event)) >>= (`shouldSatisfy` isLeft)
  it "returns the error for a file that can't be read" $
    withSystemTempDirectory "corydalis-yaml" $ \dir ->
      (loadYaml (dir </> "missing") :: IO (Either String Event))
        >>= (`shouldSatisfy` isLeft)
