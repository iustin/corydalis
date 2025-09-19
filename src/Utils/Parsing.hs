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

module Utils.Parsing ( parseDecimal
                      , parseReal
             ) where

import qualified Data.Text.Read      as Text

import           Import.NoFoundation hiding (leftover)

-- | Simpler Text to decimal parsing with error handling.
parseDecimal :: (Integral a) => Text -> Either Text a
parseDecimal w =
  case Text.signed Text.decimal w of
    Right (w', "") -> Right w'
    Right (w', leftover) ->
      Left $ sformat ("Parsed " % int % " decimal but with leftover text '" %
                      stext % "'") w' leftover
    Left msg ->
      Left $ sformat ("Failed to parse integer from '" % stext % "': " %
                      string) w msg

-- | Simpler Text to real parsing with error handling.
parseReal :: Text -> Either Text Double
parseReal w =
  case Text.signed Text.double w of
    Right (w', "") -> Right w'
    Right (w', leftover) ->
      Left $ sformat ("Parsed " % shortest % " fractional but with leftover text '" %
                      stext % "'") w' leftover
    Left msg ->
      Left $ sformat ("Failed to parse fractional from '" % stext % "': " %
                      string) w msg
