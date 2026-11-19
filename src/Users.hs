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

-- | Changes to the users, as done by @db-util@.
--
-- Sessions only hold the user id and its generation (see 'User'), so
-- the changes that should end the existing sessions of a user increase
-- its generation.
module Users ( addOrUpdateUser
             , deleteUser
             , logoutUser
             ) where

import           Database.Persist
import           Import.NoFoundation

-- | Adds a user, or updates the password of an existing one, restoring
-- it if deleted (with the same id).
--
-- For an existing user, this ends its sessions: after a password
-- change, sessions from before might have been obtained with the old
-- one, and the sessions of a deleted user (which might have been
-- deleted because it was compromised) must not come back.
addOrUpdateUser :: MonadIO m
                => User  -- ^ The user, with its new password.
                -> ReaderT SqlBackend m ()
addOrUpdateUser user =
  void $ upsertBy (UniqueUser (userName user))
                  user { userDeleted = False, userGeneration = 0 }
                  [ UserPassword =. userPassword user
                  , UserDeleted =. False
                  , UserGeneration +=. 1
                  ]

-- | Marks a user as deleted, and ends its sessions; returns whether it
-- existed.
--
-- The user is only marked as deleted, so that its id is never reused:
-- a new user with the same id would otherwise get the deleted user's
-- sessions.
deleteUser :: MonadIO m => Text -> ReaderT SqlBackend m Bool
deleteUser name =
  updateUser name [ UserDeleted =. True
                  , UserPassword =. Nothing
                  , UserGeneration +=. 1
                  ]

-- | Ends all the sessions of a user, without other changes; returns
-- whether it existed.
logoutUser :: MonadIO m => Text -> ReaderT SqlBackend m Bool
logoutUser name = updateUser name [UserGeneration +=. 1]

-- | Updates a user given by name; returns whether it existed.
updateUser :: MonadIO m => Text -> [Update User] -> ReaderT SqlBackend m Bool
updateUser name updates = do
  found <- getBy $ UniqueUser name
  case found of
    Nothing             -> return False
    Just (Entity uid _) -> True <$ update uid updates
