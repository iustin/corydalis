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
{-# LANGUAGE TemplateHaskell   #-}

-- | The encoding of the repository cache: the configuration the
-- repository was built with, and the repository, behind a format
-- version.
--
-- The version is a hash of the shape of the cached types (the names of
-- the types with their modules, of the constructors and fields, and the
-- types of the fields, from their 'Data' instances), computed at
-- compile time by "Data.Store.Version". Decoding a cache written with
-- another version fails, as does decoding bytes from before versions
-- existed, so such a cache is ignored and a scan runs.
--
-- 'repoCacheVersion' pins the expected hash (in "Repository", since a
-- value used at compile time can't be defined in the same module): a
-- change to the shape of a cached type fails the build until the pin
-- is updated. The build writes the new shape to
-- @dist/store-versioned/\<hash\>@ (under @.stack-work@ instead if
-- @STACK_EXE@ is set when compiling, which @stack build@ doesn't do);
-- the previous shape is there too if the same directory built it, and
-- the error then shows the @diff@ command. An encoding change the
-- shape doesn't show needs a new 'CacheRevision'.
module Repository.Encoding
  ( encodeRepository
  , decodeRepository
  ) where

import           Data.Store         (PeekException, decodeWith)
import           Data.Store.Core    (unsafeEncodeWith)
import           Data.Store.Version (decodeWithVersionQ, encodeWithVersionQ)

import           Import.NoFoundation
import           Repository

-- | Encodes a repository and its configuration for the cache.
encodeRepository :: (Config, Repository) -> ByteString
encodeRepository (config, repo) =
  let contents = (CacheRevision1, config, repo) :: CachedRepo
      (len, pokeIt) = $(encodeWithVersionQ repoCacheVersion) contents
  in unsafeEncodeWith pokeIt len

-- | Decodes a repository cache, failing if it has another format
-- version (or none).
decodeRepository :: ByteString -> Either PeekException (Config, Repository)
decodeRepository encoded = do
  (_, config, repo) <- decodeCached encoded
  return (config, repo)
  where decodeCached :: ByteString -> Either PeekException CachedRepo
        decodeCached = decodeWith $(decodeWithVersionQ repoCacheVersion)
