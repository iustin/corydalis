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

module Handler.SearchSpec (spec) where

import           TestImport

checkRedirAndHeader :: YesodExample App ()
checkRedirAndHeader = do
  checkRedirect
  statusIs 200
  htmlAllContain "h1" "Searching folders"

spec :: Spec
spec = withApp $ do
  -- TODO: add folders by year
  -- TODO: add quick search
  -- TODO: add search images
  -- TODO: add search folders
  describe "checks the per-year handlers" $ do
    it "loads the folders no year page and checks it looks right" $ do
      login
      get SearchFoldersNoYearR
      checkRedirAndHeader
    it "loads empty folder=1900 page and checks it looks right" $ do
      login
      get $ SearchFoldersByYearR 1900
      checkRedirAndHeader
      htmlAnyContain "div.card-header" "Nothing found"
      htmlAnyContain "div.card-body" "doesn&#39;t match any folders"
  describe "checks image search" $ do
    it "checks invalid page request" $ do
      login
      checkRouteIs (SearchImagesR (-1)) 400
    it "checks empty search on page 0" $ do
      login
      checkRoute $ SearchImagesR 0
      htmlAnyContain "div.card-header" "Nothing found"
      htmlAnyContain "div.card-body" "doesn&#39;t match any images"
      bodyContains "data-page-index=\"0\""
      bodyContains "data-initial-count=\"0\""
    it "checks empty search on page 3" $ do
      login
      checkRoute $ SearchImagesR 3
      htmlAnyContain "div.card-header" "Nothing found"
      htmlAnyContain "div.card-body" "doesn&#39;t match any images"
      bodyContains "data-page-index=\"3\""
      bodyContains "data-initial-count=\"0\""
  describe "checks quick search" $ do
    it "checks for missing search parameter" $ do
      login
      checkRouteIs QuickSearchR 400
    it "checks for invalid search parameter" $ do
      login
      get (QuickSearchR, [("q", "")])
      statusIs 400
    it "checks for fruitless search" $ do
      login
      get (QuickSearchR, [("q", "1900")])
      checkRedirect
