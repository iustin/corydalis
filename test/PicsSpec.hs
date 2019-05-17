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
import           Types        (Config, ctxConfig)

simpleImage :: Config -> Image
simpleImage config =
  let f = File "a.nef" 0 0 0 "/no-such-file" def
  in mkImage config "a" "b" (Just f) Nothing []
             Nothing [] [] Nothing MediaImage def

spec :: Spec
spec = withContext $
  describe "search cache" $ do
    it "caches a search result" $ \ctx -> do
      let image = simpleImage (ctxConfig ctx)
          m1 = Map.singleton ("a", (Nothing, "b")) image
      getSearchResults ctx m1 [] `shouldReturn` m1
      getSearchResults ctx (error "Failed to cache") [] `shouldReturn` m1


    it "flushes the search cache on rescan" $ \ctx -> do
      let image = simpleImage (ctxConfig ctx)
          m1 = Map.singleton ("a", (Nothing, "b")) image
          m2 = Map.empty
      _ <- launchScanFileSystem ctx
      getSearchResults ctx m1 [] `shouldReturn` m1
      _ <- launchScanFileSystem ctx
      getSearchResults ctx m2 [] `shouldReturn` m2
