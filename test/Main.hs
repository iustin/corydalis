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

module Main where

import           Foundation        (clientSessionKeyFile)
import qualified Spec
import           Test.Hspec.Runner
import           Web.ClientSession

main :: IO ()
main = do
  -- Ensure the key is initialised, otherwise there can be conflicting locks in
  -- the parallel test runners when run on a CI env, e.g.:
  --
  -- uncaught exception: IOException of type ResourceBusy
  -- config/client_session_key.aes: openBinaryFile: resource busy (file is locked)
  _ <- getKey clientSessionKeyFile
  -- and now run the specs
  hspec Spec.spec
