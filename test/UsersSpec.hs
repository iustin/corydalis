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

module UsersSpec (spec) where

import           TestImport
import           Users

-- | A user, with a new password; db-util hashes it (see 'setPassword'),
-- but the operations tested here store it as given.
newUser :: Text -> Text -> User
newUser name password = User { userName = name, userPassword = Just password
                             , userDeleted = False, userGeneration = 0 }

-- | Returns a user by name.
lookupUser :: Text -> YesodExample App (Maybe (Entity User))
lookupUser = runDB . getBy . UniqueUser

-- | Returns a user by name, which must exist.
getUser :: Text -> YesodExample App (Entity User)
getUser name =
  lookupUser name >>= maybe (liftIO $ throwIO $ userError "no such user") return

spec :: Spec
spec = withApp $ do
  describe "addOrUpdateUser" $ do
    it "adds a new user, at the initial generation" $ do
      runDB $ addOrUpdateUser (newUser "alice" "pw")
      Entity _ user <- getUser "alice"
      liftIO $ do
        userPassword user `shouldBe` Just "pw"
        userDeleted user `shouldBe` False
        userGeneration user `shouldBe` 0
    it "changes the password, and the generation, of an existing user" $ do
      runDB $ addOrUpdateUser (newUser "alice" "pw")
      Entity uid _ <- getUser "alice"
      runDB $ addOrUpdateUser (newUser "alice" "new")
      Entity uid' user <- getUser "alice"
      liftIO $ do
        uid' `shouldBe` uid
        userPassword user `shouldBe` Just "new"
        userGeneration user `shouldBe` 1
    it "restores a deleted user, with the same id and a new generation" $ do
      runDB $ addOrUpdateUser (newUser "alice" "pw")
      Entity uid _ <- getUser "alice"
      _ <- runDB $ deleteUser "alice"
      runDB $ addOrUpdateUser (newUser "alice" "new")
      Entity uid' user <- getUser "alice"
      liftIO $ do
        uid' `shouldBe` uid
        userDeleted user `shouldBe` False
        userPassword user `shouldBe` Just "new"
        userGeneration user `shouldBe` 2
  describe "deleteUser" $ do
    it "marks the user as deleted, without a password" $ do
      runDB $ addOrUpdateUser (newUser "alice" "pw")
      found <- runDB $ deleteUser "alice"
      Entity _ user <- getUser "alice"
      liftIO $ do
        found `shouldBe` True
        userDeleted user `shouldBe` True
        userPassword user `shouldBe` Nothing
        userGeneration user `shouldBe` 1
    it "reports an unknown user" $ do
      found <- runDB $ deleteUser "nobody"
      liftIO $ found `shouldBe` False
      missing <- isNothing <$> lookupUser "nobody"
      liftIO $ missing `shouldBe` True
  describe "logoutUser" $ do
    it "only changes the generation" $ do
      runDB $ addOrUpdateUser (newUser "alice" "pw")
      found <- runDB $ logoutUser "alice"
      Entity _ user <- getUser "alice"
      liftIO $ do
        found `shouldBe` True
        userGeneration user `shouldBe` 1
        userPassword user `shouldBe` Just "pw"
        userDeleted user `shouldBe` False
    it "reports an unknown user" $ do
      found <- runDB $ logoutUser "nobody"
      liftIO $ found `shouldBe` False
