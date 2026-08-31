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

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils.Yaml ( loadOptionalYaml
             ) where

import           Data.YAML
import           Import.NoFoundation hiding (leftover)

import qualified Data.ByteString     as BS
import qualified Data.YAML           as HsYAML
import qualified System.Directory    as Dir
-- adjust imports/types to your exact HsYAML module

loadOptionalYaml
  :: Data.YAML.FromYAML a
  => FilePath
  -> IO (Maybe String, Maybe a)
loadOptionalYaml filepath = do
  file_exists <- Dir.doesFileExist filepath
  if not file_exists
    then pure (Nothing, Nothing)
    else do
      file_bytes <- BS.readFile filepath
      case HsYAML.decode1Strict file_bytes of
        Right v -> pure (Nothing, Just v)
        Left e  -> pure (Just (show e), Nothing)
