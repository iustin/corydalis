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
--
-- Connecting to a Unix socket needs write permission on it, so a proxy
-- running as another user can only connect if the socket's group or
-- mode allow it; by default, the socket gets the process' group, and
-- its mode from the umask.
module Utils.Socket ( listenUnixSocket
                    , removeStaleSocket
                    , parseSocketMode
                    ) where

import           ClassyPrelude
import           Network.Socket     (Family (AF_UNIX), SockAddr (SockAddrUnix),
                                     Socket, SocketType (Stream), bind, close,
                                     listen, maxListenQueue, socket)
import           Numeric            (readOct)
import           System.Posix.Files (getSymbolicLinkStatus, isSocket,
                                     removeLink, setFileMode, setOwnerAndGroup)
import           System.Posix.Types (FileMode, GroupID)
import           System.Posix.User  (getGroupEntryForName, groupID)

-- | Parses a socket mode, given as an octal string (e.g. @0660@).
--
-- Only the permission bits are allowed (at most @0777@).
parseSocketMode :: Text -> Either Text FileMode
parseSocketMode txt =
  case readOct (unpack txt) of
    [(v, "")] | not (null txt) && v <= (0o777 :: Integer) ->
                Right (fromInteger v)
    _ -> Left $ "Invalid socket mode '" <> txt <>
              "': must be an octal value, at most 0777 (e.g. \"0660\")"

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

-- | Looks up a group by name.
lookupGroup :: Text -> IO GroupID
lookupGroup name =
  -- getGroupEntryForName raises an error both for a missing group and
  -- for lookup failures.
  either (\e -> throwIO . userError $ "Cannot find the group '" ++
                  unpack name ++ "' for the Unix socket: " ++
                  show (e :: IOException))
         (return . groupID)
    =<< try (getGroupEntryForName (unpack name))

-- | Sets the group and mode of the socket, if given.
setSocketAccess :: FilePath -> Maybe Text -> Maybe FileMode -> IO ()
setSocketAccess path mgroup mode = do
  forM_ mgroup $ \name -> do
    gid <- lookupGroup name
    -- Owner -1 keeps the owner.
    setOwnerAndGroup path (-1) gid `catch` \e ->
      throwIO . userError $ "Cannot set the group of the Unix socket " ++
        path ++ " to '" ++ unpack name ++ "'" ++
        (if isPermissionError e
           then " (the user running Corydalis must be a member of it)"
           else "") ++ ": " ++ show e
  forM_ mode $ setFileMode path

-- | Creates a Unix socket at the given path, listening for connections.
--
-- A stale socket at the path is removed first (see
-- 'removeStaleSocket'). The group and mode, if given, are set before
-- listening, so no connections are accepted before.
listenUnixSocket :: FilePath -> Maybe Text -> Maybe FileMode -> IO Socket
listenUnixSocket path mgroup mode = do
  removeStaleSocket path
  bracketOnError (socket AF_UNIX Stream 0) close $ \sock -> do
    bind sock (SockAddrUnix path)
    setSocketAccess path mgroup mode
    listen sock maxListenQueue
    return sock
