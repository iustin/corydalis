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

module Utils.SocketSpec (spec) where

import           Data.Either        (isLeft)
import           Network.Socket     (Family (AF_UNIX), SockAddr (SockAddrUnix),
                                     SocketType (Stream), close, connect,
                                     socket)
import           System.Directory   (createDirectory, doesDirectoryExist,
                                     doesFileExist)
import           System.Posix.Files (fileGroup, fileMode, getFileStatus,
                                     intersectFileModes)
import           System.Posix.Types (FileMode)
import           System.Posix.User  (getEffectiveGroupID, getGroupEntryForID,
                                     groupName)

import           TestImport
import           Utils.Socket

-- | Runs an action with a socket path in a new temporary directory.
withSocketPath :: (FilePath -> IO a) -> IO a
withSocketPath action =
  withSystemTempDirectory "corydalis-sock" $ \tmp -> action (tmp </> "socket")

-- | Creates a listening socket, runs the action, then closes it.
withListening :: FilePath -> Maybe Text -> Maybe FileMode -> IO a -> IO a
withListening path mgroup mode action =
  bracket (listenUnixSocket path mgroup mode) close (const action)

-- | Connects to the socket at the given path.
connectTo :: FilePath -> IO ()
connectTo path =
  bracket (socket AF_UNIX Stream 0) close (`connect` SockAddrUnix path)

-- | Returns the permission bits of a file.
permissions :: FilePath -> IO FileMode
permissions path = intersectFileModes 0o777 . fileMode <$> getFileStatus path

spec :: Spec
spec = do
  describe "parseSocketMode" $ do
    it "parses octal modes" $ do
      parseSocketMode "0660" `shouldBe` Right 0o660
      parseSocketMode "660" `shouldBe` Right 0o660
      parseSocketMode "0777" `shouldBe` Right 0o777
    it "refuses invalid modes" $
      forM_ ["", "abc", "0668", "-660", "0660 ", "1777", "4660"] $ \m ->
        parseSocketMode m `shouldSatisfy` isLeft

  describe "removeStaleSocket" $ do
    it "accepts a missing path" $ withSocketPath $ \path -> do
      removeStaleSocket path
      doesFileExist path `shouldReturn` False
    it "removes a socket" $ withSocketPath $ \path -> do
      withListening path Nothing Nothing $ return ()
      doesFileExist path `shouldReturn` True
      removeStaleSocket path
      doesFileExist path `shouldReturn` False
    it "refuses to remove a regular file" $ withSocketPath $ \path -> do
      writeFile path ("data" :: ByteString)
      removeStaleSocket path `shouldThrow` anyIOException
      doesFileExist path `shouldReturn` True
    it "refuses to remove a directory" $ withSocketPath $ \path -> do
      createDirectory path
      removeStaleSocket path `shouldThrow` anyIOException
      doesDirectoryExist path `shouldReturn` True

  describe "listenUnixSocket" $ do
    it "accepts connections" $ withSocketPath $ \path ->
      withListening path Nothing Nothing $ connectTo path
    it "replaces a stale socket" $ withSocketPath $ \path -> do
      withListening path Nothing Nothing $ return ()
      withListening path Nothing Nothing $ connectTo path
    it "refuses to replace a regular file" $ withSocketPath $ \path -> do
      writeFile path ("data" :: ByteString)
      listenUnixSocket path Nothing Nothing `shouldThrow` anyIOException
      readFile path `shouldReturn` ("data" :: ByteString)
    it "sets the mode" $ withSocketPath $ \path -> do
      withListening path Nothing (Just 0o660) $
        permissions path `shouldReturn` 0o660
      withListening path Nothing (Just 0o600) $
        permissions path `shouldReturn` 0o600
    it "sets the group" $ withSocketPath $ \path -> do
      gid <- getEffectiveGroupID
      name <- pack . groupName <$> getGroupEntryForID gid
      withListening path (Just name) Nothing $
        (fileGroup <$> getFileStatus path) `shouldReturn` gid
    it "fails for an unknown group" $ withSocketPath $ \path ->
      listenUnixSocket path (Just "no-such-group-corydalis") Nothing
        `shouldThrow` anyIOException
