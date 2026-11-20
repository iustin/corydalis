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

module Utils.PermissionsSpec (spec) where

import           Numeric            (showOct)
import           System.Directory   (createDirectory)
import           System.Posix.Files (fileMode, fileOwner, getFileStatus,
                                     intersectFileModes, setFileMode)
import           System.Posix.Types (FileMode)
import           System.Posix.User  (getEffectiveUserID)

import           TestImport
import           Utils.Permissions

-- | Returns the permission bits of a file.
permissions :: FilePath -> IO FileMode
permissions path = intersectFileModes 0o777 . fileMode <$> getFileStatus path

-- | Runs an action with a new directory having the given permissions.
withDirMode :: FileMode -> (FilePath -> IO a) -> IO a
withDirMode mode action =
  withSystemTempDirectory "corydalis-perms" $ \tmp -> do
    let dir = tmp </> "private"
    createDirectory dir
    setFileMode dir mode
    action dir

-- | Changing the mode fails, as on a read-only file system.
failingChangeMode :: FilePath -> FileMode -> IO ()
failingChangeMode _ _ = ioError $ userError "read-only file system"

-- | Whether the result is an accessible directory, for the given reason.
isAccessibleFor :: Text -> PrivateDirStatus -> Bool
isAccessibleFor reason (PrivateDirAccessible msg) = reason `isInfixOf` msg
isAccessibleFor _ _ = False

spec :: Spec
spec = do
  describe "protectPrivateDir" $ do
    it "accepts a directory accessible only to its owner" $
      withDirMode 0o700 $ \dir -> do
        protectPrivateDir dir `shouldReturn` PrivateDirOK
        permissions dir `shouldReturn` 0o700
    forM_ [0o755, 0o750, 0o711, 0o701] $ \mode ->
      it ("restricts a directory with mode " ++ showOct mode "") $
        withDirMode mode $ \dir -> do
          protectPrivateDir dir `shouldReturn` PrivateDirRestricted
          permissions dir `shouldReturn` 0o700
    it "doesn't change a private directory, even if it can't" $
      withDirMode 0o700 $ \dir ->
        protectPrivateDirWith getEffectiveUserID failingChangeMode dir
          `shouldReturn` PrivateDirOK
    it "reports a directory it can't restrict" $
      withDirMode 0o755 $ \dir -> do
        protectPrivateDirWith getEffectiveUserID failingChangeMode dir >>=
          (`shouldSatisfy` isAccessibleFor "read-only file system")
        permissions dir `shouldReturn` 0o755
    it "doesn't restrict a directory owned by another user" $
      withDirMode 0o755 $ \dir -> do
        owner <- fileOwner <$> getFileStatus dir
        protectPrivateDirWith (return (owner + 1)) setFileMode dir >>=
          (`shouldSatisfy` isAccessibleFor "not owned")
        permissions dir `shouldReturn` 0o755
    it "reports a missing directory" $
      withSystemTempDirectory "corydalis-perms" $ \tmp ->
        protectPrivateDir (tmp </> "missing") `shouldReturn` PrivateDirMissing
  describe "privateDirMessage" $ do
    it "says nothing for private or missing directories" $ do
      privateDirMessage "db" PrivateDirOK `shouldBe` Nothing
      privateDirMessage "db" PrivateDirMissing `shouldBe` Nothing
    it "names the directory and the reason" $ do
      privateDirMessage "db" PrivateDirRestricted
        `shouldSatisfy` maybe False ("directory db " `isInfixOf`)
      let msg = privateDirMessage "db" (PrivateDirAccessible "some reason")
      msg `shouldSatisfy` maybe False ("directory db " `isInfixOf`)
      msg `shouldSatisfy` maybe False ("some reason" `isInfixOf`)
  describe "databaseDirectory" $ do
    it "returns the directory of database files" $ do
      databaseDirectory "db/corydalis.sqlite3" `shouldBe` Just "db"
      databaseDirectory "/var/lib/corydalis/db.sqlite3" `shouldBe` Just "/var/lib/corydalis"
    it "ignores in-memory databases and URIs" $ do
      databaseDirectory ":memory:" `shouldBe` Nothing
      databaseDirectory "file:db/corydalis.sqlite3?mode=ro" `shouldBe` Nothing
    it "ignores the current directory" $
      databaseDirectory "corydalis.sqlite3" `shouldBe` Nothing
  describe "databaseFileDirectory" $ do
    it "returns the directory of database files" $ do
      databaseFileDirectory "db/corydalis.sqlite3" `shouldBe` Just "db"
      databaseFileDirectory "corydalis.sqlite3" `shouldBe` Just "."
    it "ignores in-memory databases and URIs" $ do
      databaseFileDirectory ":memory:" `shouldBe` Nothing
      databaseFileDirectory "file:db/corydalis.sqlite3?mode=ro" `shouldBe` Nothing
