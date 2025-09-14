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

module Handler.BrowseSpec (spec) where

import           TestImport

import           Indexer

spec :: Spec
spec = parallel $ withApp $ do
  describe "checks folder browsing" $
    it "checks invalid page request" $ do
      login
      checkRouteIs (BrowseImagesR (-1)) 400
  describe "checks image browsing" $ do
    it "checks invalid page request" $ do
      login
      checkRouteIs (BrowseImagesR (-1)) 400
    it "checks empty search on page 0" $ do
      login
      checkRoute $ BrowseImagesR 0
      htmlAnyContain "div.card-header" "Nothing found"
      htmlAnyContain "div.card-body" "doesn't match any images"
      bodyContains "data-page-index=\"0\""
      bodyContains "data-initial-count=\"0\""
    it "checks empty search on page 3" $ do
      login
      checkRoute $ BrowseImagesR 3
      htmlAnyContain "div.card-header" "Nothing found"
      htmlAnyContain "div.card-body" "doesn't match any images"
      bodyContains "data-page-index=\"3\""
      bodyContains "data-initial-count=\"0\""
  describe "checks that wrong filter image search shows search impossible" $
    forM_ [minBound..maxBound] $ \fclass ->
      it ("validates folder class " ++ show fclass) $ do
        login
        checkRouteIsWithParams (BrowseImagesR 0) (atomToParams (FClass fclass)) 200
        bodyContains "not able to match files"
