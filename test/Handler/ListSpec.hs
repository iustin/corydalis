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

module Handler.ListSpec (spec) where

import           TestImport

import           Indexer

spec :: Spec
spec = parallel $ withApp $ do
  describe "loads the list pages and checks it looks right" $
    forM_ [minBound..maxBound] $ \symbol ->
      it ("validates symbol " ++ show symbol) $ do
        login
        checkRoute $ ListItemsR symbol
        htmlAllContain "h1" "Listing"
  it "loads empty folder list page" $ do
    login
    checkRoute ListFoldersR
    bodyContains "Nothing found"
  describe "checks that wrong filter image list shows search impossible" $
    forM_ [minBound..maxBound] $ \fclass ->
      it ("validates folder class " ++ show fclass) $ do
        login
        checkRouteIsWithParams ListImagesR (atomToParams (FClass fclass)) 200
        bodyContains "not able to match files"
