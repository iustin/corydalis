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

import           Indexer    (Symbol (TPath))

spec :: Spec
spec = withApp $ do
  describe "loads the list pages and checks it looks right" $ do
    forM_ [s | s <- [minBound..maxBound], s /= TPath] $ \symbol ->
      it ("validates symbol " ++ show symbol) $ do
        login
        checkRoute $ ListItemsR symbol
        htmlAllContain "h1" "Listing"
    it "checks Paths are not listable" $ do
      login
      checkNotFound $ ListItemsR TPath
  it "loads wrong folder browse page and checks it 404's" $ do
    login
    checkNotFound $ ListFoldersR []
  it "loads wrong image browse page and checks it 404's" $ do
    login
    checkNotFound $ BrowseImagesR []
