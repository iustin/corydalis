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

module Utils.SessionSpec (spec) where

import qualified Data.Map      as Map
import           Network.Wai   (defaultRequest)
import           Yesod.Core.Types (SessionBackend (..), SessionMap)

import           TestImport
import           Utils.Session

-- | Saves the given session through a wrapped backend, returning what
-- the underlying backend saved.
saveThroughWrapper :: SessionMap -> IO SessionMap
saveThroughWrapper sess = do
  saved <- newIORef Map.empty
  let backend = SessionBackend
        { sbLoadSession = \_ ->
            return (Map.empty, \m -> writeIORef saved m >> return [])
        }
  wrapped <- rotatingCsrfSessions (return (Just backend))
  case wrapped of
    Nothing -> expectationFailure "The wrapper lost the backend"
    Just sb -> do
      (_, save) <- sbLoadSession sb defaultRequest
      void $ save sess
  readIORef saved

spec :: Spec
spec =
  describe "rotatingCsrfSessions" $ do
    let old = Map.fromList [(csrfTokenKey, "old"), ("other", "value")]
    it "keeps the token of an unmarked session" $
      saveThroughWrapper old `shouldReturn` old
    it "drops the token and the mark of a marked session" $
      saveThroughWrapper (Map.insert newCsrfTokenKey "" old)
        `shouldReturn` Map.singleton "other" "value"
    it "drops the mark of a marked session without a token" $
      saveThroughWrapper (Map.singleton newCsrfTokenKey "")
        `shouldReturn` Map.empty
