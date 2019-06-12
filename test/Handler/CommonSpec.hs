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

module Handler.CommonSpec (spec) where

import           TestImport

spec :: Spec
spec = parallel $ withApp $ do
  describe "robots.txt" $ do
    it "gives a 200" $
      checkRoute RobotsR
    it "has correct User-agent" $ do
      checkRoute RobotsR
      bodyContains "User-agent: *"
  describe "favicon.ico" $
    it "gives a 200" $ do
      get FaviconR
      statusIs 200
