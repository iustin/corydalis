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

import           Network.Socket     (Family (AF_UNIX), SockAddr (SockAddrUnix),
                                     SocketType (Stream), close, connect,
                                     socket)
import           System.Directory   (createDirectory, doesDirectoryExist,
                                     doesFileExist)

import           TestImport
import           Utils.Socket

-- | Runs an action with a socket path in a new temporary directory.
withSocketPath :: (FilePath -> IO a) -> IO a
withSocketPath action =
  withSystemTempDirectory "corydalis-sock" $ \tmp -> action (tmp </> "socket")

-- | Creates a listening socket, runs the action, then closes it.
withListening :: FilePath -> IO a -> IO a
withListening path action =
  bracket (listenUnixSocket path) close (const action)

-- | Connects to the socket at the given path.
connectTo :: FilePath -> IO ()
connectTo path =
  bracket (socket AF_UNIX Stream 0) close (`connect` SockAddrUnix path)

spec :: Spec
spec = do
  describe "removeStaleSocket" $ do
    it "accepts a missing path" $ withSocketPath $ \path -> do
      removeStaleSocket path
      doesFileExist path `shouldReturn` False
    it "removes a socket" $ withSocketPath $ \path -> do
      withListening path $ return ()
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
      withListening path $ connectTo path
    it "replaces a stale socket" $ withSocketPath $ \path -> do
      withListening path $ return ()
      withListening path $ connectTo path
    it "refuses to replace a regular file" $ withSocketPath $ \path -> do
      writeFile path ("data" :: ByteString)
      listenUnixSocket path `shouldThrow` anyIOException
      readFile path `shouldReturn` ("data" :: ByteString)
