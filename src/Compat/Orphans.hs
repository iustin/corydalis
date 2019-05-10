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

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Compat.Orphans
where

import           Data.Store          ()
import           Data.Store.TH       (makeStore)

import           Control.DeepSeq
import           Data.Time.Clock
import           Data.Time.LocalTime
import           System.Posix.Types

$(makeStore ''TimeOfDay)
$(makeStore ''LocalTime)
$(makeStore ''NominalDiffTime)
$(makeStore ''COff)

instance NFData COff where
  rnf (COff x) = rnf x

$(makeStore ''TimeZone)
$(makeStore ''ZonedTime)
