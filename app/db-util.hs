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

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad.Logger
import           Data.Text.IO
import           Database.Persist
import           Database.Persist.Sqlite
import           Import                  hiding (getLine, hFlush, hGetEcho,
                                          hSetEcho, putStr, putStrLn)
import           Options.Applicative
import           System.IO               (hFlush, hGetEcho, hSetEcho)
import           Users                   (addOrUpdateUser, deleteUser,
                                          logoutUser)
import           Utils.Permissions       (databaseDirectory, privateDirMessage,
                                          protectPrivateDir)
import           Yesod.Auth.HashDB

data Options = Options
  { optSettingsFile :: FilePath
  , optCommand      :: Command
  }

data Command = CmdAdd Text
             | CmdDel Text
             | CmdLogout Text
             | CmdList

parseAdd :: Parser Command
parseAdd = CmdAdd <$> argument str (metavar "USERNAME")

parseDel :: Parser Command
parseDel = CmdDel <$> argument str (metavar "USERNAME")

parseLogout :: Parser Command
parseLogout = CmdLogout <$> argument str (metavar "USERNAME")

parseList :: Parser Command
parseList = pure CmdList

parseCommand :: Parser Command
parseCommand = subparser $
  command "add" (info parseAdd (progDesc "Add/update a user")) <>
  command "del" (info parseDel (progDesc "Remove a user")) <>
  command "logout" (info parseLogout
                    (progDesc "End all the sessions of a user")) <>
  command "list" (info parseList (progDesc "List the users"))

parseOptions :: Parser Options
parseOptions = Options
  <$> strOption (long "config" <>
                 short 'c' <>
                 metavar "FILE" <>
                 value "config/settings.yml" <>
                 help "Load configuration settings from FILE"
                )
  <*> parseCommand

upsertUser :: MonadIO m
           => Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
upsertUser username = do
  password <- liftIO $ do
    putStr "Enter password: "
    hFlush stdout
    bracket (hGetEcho stdin) (hSetEcho stdin) $ \_ -> do
      hSetEcho stdin False
      l <- getLine
      putChar '\n'
      return l
  let u = User { userName = username, userPassword = Nothing
               , userDeleted = False, userGeneration = 0 }
  withpw <- liftIO $ setPassword password u
  -- Adding a deleted user restores it, with the same id; both this and a
  -- password change end the existing sessions of the user.
  addOrUpdateUser withpw

-- | Applies a change to an existing user, warning if it doesn't exist.
changeUser :: MonadIO m
           => (Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) Bool)
           -> Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
changeUser change user = do
  found <- change user
  unless found . liftIO . hPutStrLn stderr $
    concat ["User ", user, " not found. Ignoring."]

listUsers :: MonadIO m
          => ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
listUsers = do
  users <- selectList [] [Asc UserName]
  liftIO $ mapM_ (\(Entity _ user) ->
                    putStrLn $ if userDeleted user
                                 then userName user <> " (deleted)"
                                 else userName user
                 ) users

app :: MonadIO m
    => Command -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
app cmd = do
  runMigration migrateAll
  case cmd of
    CmdAdd u -> upsertUser u
    CmdDel u -> changeUser deleteUser u
    CmdLogout u -> changeUser logoutUser u
    CmdList  -> listUsers

main :: IO ()
main = do
    opts <- execParser (info (helper <*> parseOptions) $ progDesc "Corydalis database maintenance")

    -- Get the settings from all relevant sources
    settings <- loadYamlSettings [optSettingsFile opts] [] useEnv
    let conn = sqlDatabase $ appDatabaseConf settings

    -- Restrict the database directory, as the main application does,
    -- since this might create the database.
    forM_ (databaseDirectory conn) $ \dir ->
      protectPrivateDir dir >>= mapM_ (hPutStrLn stderr) . privateDirMessage dir

    runStderrLoggingT . runSqlite conn . app . optCommand $ opts
