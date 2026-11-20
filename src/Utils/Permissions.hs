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

-- | Restricting access to the directories holding private files.
--
-- The configuration directory holds the session key, which anyone can
-- use to log in as any user, and possibly the TLS key, and the database
-- directory holds the users' password hashes. Rather than ensuring that
-- each of these files is private (they are created by libraries, with the
-- default permissions), the directories holding them are made accessible
-- only to their owner.
module Utils.Permissions ( PrivateDirStatus(..)
                         , protectPrivateDir
                         , protectPrivateDirWith
                         , privateDirMessage
                         , databaseDirectory
                         , databaseFileDirectory
                         ) where

import           Data.Bits           ((.&.), (.|.))
import           System.FilePath     (takeDirectory)
import           System.Posix.Files  (fileMode, fileOwner, getFileStatus,
                                      groupModes, otherModes, ownerModes,
                                      setFileMode)
import           System.Posix.Types  (FileMode, UserID)
import           System.Posix.User   (getEffectiveUserID)

import           Import.NoFoundation

-- | The result of checking a private directory.
data PrivateDirStatus
  = PrivateDirOK
    -- ^ The directory is only accessible to its owner.
  | PrivateDirRestricted
    -- ^ The directory was accessible to others, and has been restricted.
  | PrivateDirAccessible Text
    -- ^ The directory is accessible to others, and could not be
    -- restricted, for the given reason.
  | PrivateDirMissing
    -- ^ The directory doesn't exist (yet).
  deriving (Eq, Show)

-- | Restricts a directory to its owner, if needed and possible.
--
-- This changes nothing if the directory is already private, so it works
-- for read-only directories; otherwise, it only changes the directories
-- owned by the user running the application, since the others are
-- presumably set up deliberately by the administrator.
protectPrivateDir :: FilePath -> IO PrivateDirStatus
protectPrivateDir = protectPrivateDirWith getEffectiveUserID setFileMode

-- | Version of 'protectPrivateDir' taking the functions to get the
-- effective user ID and change the mode, for testing.
protectPrivateDirWith :: IO UserID
                      -> (FilePath -> FileMode -> IO ())
                      -> FilePath -> IO PrivateDirStatus
protectPrivateDirWith getUid changeMode dir = do
  -- Errors other than a missing directory (e.g. permission denied on a
  -- parent directory, or a symbolic link loop) are deliberately not
  -- caught: they would also prevent using the files in the directory,
  -- so the application would fail shortly afterwards anyway, and this
  -- error, naming the directory, is the clearest one.
  status <- tryJust (guard . isDoesNotExistError) $ getFileStatus dir
  case status of
    Left ()  -> return PrivateDirMissing
    Right st
      | isPrivate st -> return PrivateDirOK
      | otherwise -> do
          uid <- getUid
          if fileOwner st /= uid
            then return $ PrivateDirAccessible
                   "it is not owned by the user running the application"
            else do
              result <- tryIO $ changeMode dir ownerModes
              return $ case result of
                Right () -> PrivateDirRestricted
                Left err -> PrivateDirAccessible $
                  sformat ("restricting it failed: " % shown) err
  where isPrivate st = fileMode st .&. (groupModes .|. otherModes) == 0

-- | Describes the result of checking a private directory, if needed.
privateDirMessage :: FilePath -> PrivateDirStatus -> Maybe Text
privateDirMessage _ PrivateDirOK = Nothing
privateDirMessage _ PrivateDirMissing = Nothing
privateDirMessage dir PrivateDirRestricted = Just $
  sformat ("Restricted the directory " % string % " to its owner, as it holds \
           \private files; if other users could have read them, see the \
           \installation documentation") dir
privateDirMessage dir (PrivateDirAccessible reason) = Just $
  sformat ("The directory " % string % " holds private files, but is \
           \accessible to other users, and " % stext % "; see the \
           \installation documentation") dir reason

-- | The directory holding the database, if it should be made private.
--
-- In-memory databases, and URIs (which can hold arbitrary options), are
-- ignored, as is the current directory: it holds other files too, so
-- the database should be in its own directory.
databaseDirectory :: Text -> Maybe FilePath
databaseDirectory db = case databaseFileDirectory db of
  Just "." -> Nothing
  dir      -> dir

-- | The directory holding the database file, including the current
-- directory, if the database is a file.
--
-- In-memory databases, and URIs (which can hold arbitrary options), are
-- ignored.
databaseFileDirectory :: Text -> Maybe FilePath
databaseFileDirectory db
  | db == ":memory:" || "file:" `isPrefixOf` db = Nothing
  | otherwise = Just . takeDirectory $ unpack db
