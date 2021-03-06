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
import           Yesod.Auth.HashDB

data Options = Options
  { optSettingsFile :: FilePath
  , optCommand      :: Command
  }

data Command = CmdAdd Text
             | CmdDel Text
             | CmdList

parseAdd :: Parser Command
parseAdd = CmdAdd <$> argument str (metavar "USERNAME")

parseDel :: Parser Command
parseDel = CmdDel <$> argument str (metavar "USERNAME")

parseList :: Parser Command
parseList = pure CmdList

parseCommand :: Parser Command
parseCommand = subparser $
  command "add" (info parseAdd (progDesc "Add/update a user")) <>
  command "del" (info parseDel (progDesc "Remove a user")) <>
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
  let u = User username Nothing
  withpw <- liftIO $ setPassword password u
  _ <- upsertBy (UniqueUser username) withpw [UserPassword =. userPassword withpw]
  return ()

removeUser :: MonadIO m
           => Text -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
removeUser user = do
  userId <- getBy $ UniqueUser user
  case userId of
    Nothing -> liftIO $ hPutStrLn stderr $ concat ["User ", user, " not found. Ignoring."]
    Just (Entity uId _) -> delete uId
  return ()

listUsers :: MonadIO m
          => ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
listUsers = do
  users <- selectList [] [Asc UserName]
  liftIO $ mapM_ (\(Entity _ (User name _)) -> putStrLn name
                 ) users

app :: MonadIO m
    => Command -> ReaderT SqlBackend (NoLoggingT (ResourceT m)) ()
app cmd = do
  runMigration migrateAll
  case cmd of
    CmdAdd u -> upsertUser u
    CmdDel u -> removeUser u
    CmdList  -> listUsers

main :: IO ()
main = do
    opts <- execParser (info (helper <*> parseOptions) $ progDesc "Corydalis database maintenance")

    -- Get the settings from all relevant sources
    settings <- loadYamlSettings [optSettingsFile opts] [] useEnv
    let conn = sqlDatabase $ appDatabaseConf settings

    runStderrLoggingT . runSqlite conn . app . optCommand $ opts
