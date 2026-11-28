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

{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}


module Compat.Orphans
where

import           ClassyPrelude.Yesod                 (PathPiece (..),
                                                      PersistValue (..), Text)
import           Control.DeepSeq
import           Data.Data                           (Data)
import           Data.Functor.Contravariant
import           Data.Store
import           Data.Text.Short
import           Database.Persist.Class.PersistField
import           Database.Persist.Sql                (PersistFieldSql,
                                                      SqlType (..), sqlType)
import           System.Posix.Types
import           Text.Blaze                          (ToMarkup (toMarkup))

instance NFData COff where
  rnf (COff x) = rnf x

-- | Needed by the repository cache's format version, which derives
-- the version from the 'Data' instances of the cached types (sizes
-- are a 'FileOffset'). To remove if base gets one.
deriving instance Data COff

instance PersistField ShortText where
  toPersistValue = PersistText . toText
  fromPersistValue (PersistText t) = Right $ fromText t
  fromPersistValue _               = Left "Invalid text value for ShortText"

instance PersistFieldSql ShortText where
  sqlType _ = SqlString

-- | Encodes the text. The format versions of the repository cache and
-- of the binary exif cache don't see this encoding, only the name of
-- the type: if it changes, rename 'Repository.CacheRevision' and
-- 'Exif.BExifRevision'.
instance Store ShortText where
  size = contramap toText (size :: Size Text)
  poke = poke . toText
  peek = fromText <$> (peek::Peek Text)

instance ToMarkup ShortText where
  toMarkup = toMarkup . toText

instance PathPiece ShortText where
  fromPathPiece = Just . fromText
  toPathPiece = toText
