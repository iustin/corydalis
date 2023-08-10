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

module TypesSpec (spec) where

import           TestImport
import           Types

import           Data.Set

allViewModes :: [ViewMode]
allViewModes = [ ViewImages PresentationGrid
               , ViewImages PresentationList
               , ViewFolders PresentationGrid
               , ViewFolders PresentationList
               ]

spec :: Spec
spec = parallel $ do
  describe "checks view mode encoding/decoding" $ do
    forM_ allViewModes $ \symbol ->
      it ("validates symbol " ++ show symbol) $ do
         let parsed = parseViewMode . decodeUtf8 . formatViewMode $ symbol
         parsed `shouldBe` Just symbol
    it "checks invalid value decoding" $ do
      parseViewMode "foobar" `shouldBe` Nothing
      parseViewMode "" `shouldBe` Nothing
  describe "checks image sizes config parsing" $ withContext $ do
    it "checks duplicates between auto and on-demand" $ \ctx -> do
      let c = ctxConfig ctx
          common = cfgAutoImageSizes c  `intersect` cfgOnDemandSizes c
      common `shouldBe` Data.Set.empty
