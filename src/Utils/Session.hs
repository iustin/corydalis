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

-- | Replacing the CSRF token, e.g. at login and logout.
--
-- Yesod keeps the CSRF token in the session, and keeps it for as long
-- as the session exists, including across logins. If someone could
-- learn or set the token of a session before login (e.g. by planting a
-- session cookie in the browser), they could forge requests as the user
-- who then logs in; replacing the token at login prevents that.
--
-- Handlers can't replace it themselves: Yesod hides the token from the
-- session seen by handlers, and writes the request's token back when
-- saving the session. So handlers mark the session instead (see
-- 'requestNewCsrfToken'), and the session backend, wrapped with
-- 'rotatingCsrfSessions', drops the token when saving it. Yesod then
-- generates a new token at the next request, as for any new session;
-- login and logout end with a redirect, so that is the next page.
--
-- This can go once Yesod can replace the token itself, see
-- <https://github.com/yesodweb/yesod/issues/1939> (and the older
-- <https://github.com/yesodweb/yesod/issues/1546>).
module Utils.Session ( rotatingCsrfSessions
                     , requestNewCsrfToken
                     , newCsrfTokenKey
                     , csrfTokenKey
                     ) where

import qualified Data.Map            as Map

import           Import.NoFoundation

-- | The session key holding the CSRF token, as used by Yesod.
--
-- Yesod doesn't export it; if it changed, the token would no longer be
-- dropped, which the login tests would show.
csrfTokenKey :: Text
csrfTokenKey = "_TOKEN"

-- | The session key marking that the CSRF token is to be replaced.
newCsrfTokenKey :: Text
newCsrfTokenKey = "_NEW_TOKEN"

-- | Requests a new CSRF token, from the request after the one in which
-- the session is next saved (normally the current one).
--
-- The current request still uses (and accepts) the old token.
requestNewCsrfToken :: MonadHandler m => m ()
requestNewCsrfToken = setSession newCsrfTokenKey ""

-- | Wraps a session backend, so that sessions marked by
-- 'requestNewCsrfToken' are saved without their CSRF token (and without
-- the mark), making Yesod generate a new token at the next request.
rotatingCsrfSessions :: IO (Maybe SessionBackend)
                     -> IO (Maybe SessionBackend)
rotatingCsrfSessions = fmap (fmap wrap)
  where
    wrap sb = sb { sbLoadSession = \req -> do
                     (sess, save) <- sbLoadSession sb req
                     return (sess, save . dropToken)
                 }
    dropToken sess
      | newCsrfTokenKey `Map.member` sess =
          Map.delete csrfTokenKey $ Map.delete newCsrfTokenKey sess
      | otherwise = sess
