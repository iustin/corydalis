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

module Handler.FlaggedImagesSpec (spec) where

import           Network.HTTP.Types.Header

import           TestImport

deleteWithType :: ByteString -> YesodExample App ()
deleteWithType kind = do
  loginAndCheckFlagged
  request $ do
    setMethod "DELETE"
    setUrl $ ImageFlagR "a" "b"
    addRequestHeader (hReferer, "/")
    addRequestHeader (hAccept, kind)

loginAndCheckFlagged :: YesodExample App ()
loginAndCheckFlagged = do
  login
  checkRoute FlaggedImagesR
  bodyContains "0 flagged images"


spec :: Spec
spec = parallel $ withApp $ do
  it "loads the flagged image page and checks it looks right"
    loginAndCheckFlagged
  it "checks that flagging a non-existing image fails" $ do
    loginAndCheckFlagged
    performMethod "PUT" $ ImageFlagR "a" "b"
    statusIs 404
  it "checks that flagging an image results in 1 image flagged" $ do
    -- TODO: also add json checks when having image
    liftIO $ pendingWith "Needs repository with images"
    loginAndCheckFlagged
    performMethod "PUT" $ ImageFlagR "a" "b"
    followRedirectOK
    bodyContains "1 flagged images"
  describe "checks that de-flagging a non-existing image does not error out" $ do
    it "checks html" $ do
      deleteWithType "text/html"
      followRedirectOK
      bodyContains "Image was not flagged!"
    it "checks json" $ do
      deleteWithType "application/json"
      statusIs 200
      bodyContains "Image was not flagged!"
