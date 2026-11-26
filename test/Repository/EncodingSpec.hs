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

module Repository.EncodingSpec (spec) where

import           Data.Default
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map         as Map
import qualified Data.Store

import           Repository
import           Repository.Encoding
import           TestImport

-- | A repository with one folder holding a composite image and its
-- component, and its configuration.
sampleRepository :: Config -> (Config, Repository)
sampleRepository config =
  let image name files range =
        mkImage config name "test" Nothing Nothing (map simpleFile files) Nothing [] [] range MediaImage def
      dirs = foldl' addImageToRepo Map.empty
               [ image "d_01" ["d_01.jpg"] Nothing
               , image "d_01-02" ["d_01-02.jpg"] (Just (ImageRange "d_" 1 2 2))
               ]
  in (config, startupRepository { repoDirs = dirs, repoSerial = 5 })

-- | Decodes a repository cache, keeping only whether it worked, and
-- what was decoded, shown (as 'Repository' has no 'Eq' instance).
decodeShown :: ByteString -> Maybe (Config, String)
decodeShown =
  either (const Nothing) (\(c, r) -> Just (c, show r)) . decodeRepository

spec :: Spec
spec = parallel $ withConfig $ do
  describe "encodeRepository and decodeRepository" $ do
    it "decode what was encoded" $ \config -> do
      let (c, repo) = sampleRepository config
          roles = map imgRole . Map.elems . pdImages =<< Map.elems (repoDirs repo)
      -- The sample has the links, to check that they are encoded.
      roles `shouldMatchList` [ComponentOf ("d_01-02" :| []), CompositeOf ["d_01"]]
      decodeShown (encodeRepository (c, repo)) `shouldBe` Just (c, show repo)
    it "refuse another format version" $ \config -> do
      let encoded = encodeRepository (sampleRepository config)
      decodeShown (otherCacheVersion encoded) `shouldBe` Nothing
    it "refuse a cache from before format versions" $ \config ->
      decodeShown (Data.Store.encode (sampleRepository config))
        `shouldBe` Nothing
