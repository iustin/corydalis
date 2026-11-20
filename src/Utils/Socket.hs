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

-- | Listening on a Unix socket, for reverse-proxied deployments.
module Utils.Socket ( listenUnixSocket
                    , removeStaleSocket
                    ) where

import           ClassyPrelude
import           Network.Socket     (Family (AF_UNIX), SockAddr (SockAddrUnix),
                                     Socket, SocketType (Stream), bind, close,
                                     listen, maxListenQueue, socket)
import           System.Posix.Files (getSymbolicLinkStatus, isSocket,
                                     removeLink)

-- | Removes a socket left behind at the given path (e.g. by a previous
-- run), so that it can be bound again.
--
-- Anything else at that path (e.g. a regular file, due to a mistake in
-- the configuration) is not touched, and an error is raised instead.
removeStaleSocket :: FilePath -> IO ()
removeStaleSocket path = do
  status <- tryJust (guard . isDoesNotExistError) (getSymbolicLinkStatus path)
  case status of
    Left ()                -> return ()
    Right st | isSocket st -> removeLink path
             | otherwise   ->
                 throwIO . userError $ "The Unix socket path " ++ path ++
                   " exists and is not a socket, refusing to remove it"

-- | Creates a Unix socket at the given path, listening for connections.
--
-- A stale socket at the path is removed first (see
-- 'removeStaleSocket').
listenUnixSocket :: FilePath -> IO Socket
listenUnixSocket path = do
  removeStaleSocket path
  bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
    bind sock (SockAddrUnix path)
    listen sock maxListenQueue
    return sock
