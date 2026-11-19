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

module Utils.Yaml ( loadYaml
                  , maxYamlSize
                  ) where

import           Data.YAML
import           Import.NoFoundation hiding (leftover)

import qualified Data.ByteString     as BS
import qualified Data.YAML           as HsYAML

class YAMLWithSource a where
  setSource :: FilePath -> a -> a

instance YAMLWithSource Event where
  setSource fp ev = ev { eventSource = EventExplicit (Just fp) }

-- | The largest YAML file loaded; the per-folder files are a few lines.
maxYamlSize :: Int
maxYamlSize = 64 * 1024

-- | Loads a YAML file, returning the error if it can't be read or
-- parsed.
--
-- This reads the whole file, so callers should check first that it is
-- a regular file of a reasonable size (see 'maxYamlSize').
loadYaml
  :: (Data.YAML.FromYAML a, YAMLWithSource a)
  => FilePath
  -> IO (Either String a)
loadYaml filepath = do
  file_bytes <- try $ BS.readFile filepath
  pure $ case file_bytes of
    Left e -> Left $ show (e :: IOException)
    Right bytes' -> case HsYAML.decode1Strict bytes' of
      Right v -> Right (setSource filepath v)
      Left e  -> Left (show e)
