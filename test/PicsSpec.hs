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
{-# LANGUAGE OverloadedStrings #-}

module PicsSpec (spec) where

import           Data.Default
import qualified Data.Map     as Map

import           Pics
import           TestImport
import           Types        (Config)

simpleImage :: Config -> Image
simpleImage config =
  let f = File "a.nef" 0 0 0 "/no-such-file/a.nef" (Left "error")
  in mkImage config "a" "b" (Just f) Nothing [] Nothing def

spec :: Spec
spec = withConfig $
  describe "search cache" $ do
    it "caches a search result" $ \config -> do
      let image = simpleImage config
          m1 = Map.singleton ("a", "b") image
      getSearchResults m1 [] `shouldReturn` m1
      getSearchResults (error "Failed to cache") [] `shouldReturn` m1

    it "flushes the search cache on rescan" $ \config -> do
      let image = simpleImage config
          m1 = Map.singleton ("a", "b") image
          m2 = Map.empty
      _ <- forceScanAll config
      getSearchResults m1 [] `shouldReturn` m1
      _ <- forceScanAll config
      getSearchResults m2 [] `shouldReturn` m2
