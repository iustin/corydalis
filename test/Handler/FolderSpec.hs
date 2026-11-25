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

module Handler.FolderSpec (spec) where

import qualified Data.ByteString as BS
import           Repository.Scan (launchScanFileSystem, waitForScan)

import           Handler.Utils   (getStoragePathRenderer)
import           TestImport

-- | Adds a folder with an image and the given @corydalis.yaml@.
addEventFolder :: ShortText -> ByteString -> YesodExample App FilePath
addEventFolder name yaml = do
  dir <- addScannedFolder name ["a.jpg"]
  ctx <- appContext <$> getTestYesod
  liftIO $ do
    BS.writeFile (dir </> "corydalis.yaml") yaml
    launchScanFileSystem ctx
    void $ waitForScan ctx
  return dir

spec :: Spec
spec = parallel $ withApp $ do
  describe "folder events" $ do
    forM_ [ ("generic" :: Text, "Event"), ("birthday", "Birthday"), ("getaway", "Getaway" :: Text)
          , ("grandvacation", "Grand vacation"), ("worktrip", "Work trip") ] $
      \(kind, label) -> it ("shows " <> unpack kind <> " events") $ do
        _ <- addEventFolder "2020-02-02-event" $
               "name: The event\nkind: " <> encodeUtf8 kind <> "\npeople: [Ann]\n"
        login
        get $ FolderR "2020-02-02-event"
        statusIs 200
        bodyContains (unpack label <> ":")
        bodyContains "The event"
        bodyContains "Ann"
        bodyContains "This event was explicitly defined in"
    it "shows events without people" $ do
      _ <- addEventFolder "2020-02-02-event" "name: Alone\nkind: generic\n"
      login
      get $ FolderR "2020-02-02-event"
      statusIs 200
      bodyContains "No people annotated for this event."
  it "loads a folder page and checks it looks right" $ do
    login
    checkNotFound $ FolderR "foo"
  it "shows the full storage paths to logged in users" $ do
    dir <- addScannedFolder "2020-01-01-trip" ["a.jpg"]
    login
    get $ FolderR "2020-01-01-trip"
    statusIs 200
    bodyContains dir
  it "shows relative storage paths to anonymous users" $ do
    dir <- addScannedFolder "2020-01-01-trip" ["a.jpg"]
    access <- runHandler hasAccessToStoragePaths
    liftIO $ access `shouldBe` False
    showPath <- runHandler getStoragePathRenderer
    liftIO $ showPath dir `shouldBe` "output: 2020/2020-01-01-trip"
