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
{-# LANGUAGE OverloadedStrings #-}


module Compat.Orphans
where

import           ClassyPrelude.Yesod                 (PathPiece (..),
                                                      PersistValue (..), Text)
import           Control.DeepSeq
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

instance PersistField ShortText where
  toPersistValue = PersistText . toText
  fromPersistValue (PersistText t) = Right $ fromText t
  fromPersistValue _               = Left "Invalid text value for ShortText"

instance PersistFieldSql ShortText where
  sqlType _ = SqlString

instance Store ShortText where
  size = contramap toText (size :: Size Text)
  poke = poke . toText
  peek = fromText <$> (peek::Peek Text)

instance ToMarkup ShortText where
  toMarkup = toMarkup . toText

instance PathPiece ShortText where
  fromPathPiece = Just . fromText
  toPathPiece = toText
