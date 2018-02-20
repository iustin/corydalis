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

{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Settings.Development where

import           Data.Text
import           Data.Text.IO
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Prelude

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development

-- | Suffix to use for file names which depend on version.
--
-- The idea is to be able to share a cache between production and
-- development, and have both versions running, without conflicts for
-- files whose format changes over time (like the binary exif cache
-- files).
devSuffix :: String
devSuffix =
#if DEVELOPMENT
  "-dev"
#else
  ""
#endif

gitVersion :: String
gitVersion =
  $(addDependentFile "git-version" >>
    runIO ((unpack . strip) <$> Data.Text.IO.readFile "git-version") >>=
    stringE)

buildDate :: String
buildDate = __DATE__

corydalisVersion :: String
corydalisVersion = gitVersion ++ ", built on " ++ buildDate ++ "."
