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
  token <- pageCsrfToken
  request $ do
    setMethod "DELETE"
    setUrl $ ImageFlagR "a" "b"
    addRequestHeader (hReferer, "/")
    addRequestHeader (hAccept, kind)
    addCsrfHeader token

-- | Sends what a browser does for the image flag form: a POST with the
-- fields in document order, the method being in the first one.
postForm :: ByteString -> Maybe Text -> YesodExample App ()
postForm method token = request $ do
  setMethod "POST"
  setUrl $ ImageFlagR "a" "b"
  addRequestHeader (hReferer, "/")
  addRequestHeader (hAccept, "text/html")
  addRequestHeader (hContentType, "application/x-www-form-urlencoded")
  setRequestBody $ fromStrict $ "_method=" <> method <>
    maybe "" (("&_token=" <>) . encodeUtf8) token

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
    performWithToken "PUT" $ ImageFlagR "a" "b"
    statusIs 404
  it "checks that flagging an image results in 1 image flagged" $ do
    -- TODO: also add json checks when having image
    liftIO $ pendingWith "Needs repository with images"
    loginAndCheckFlagged
    performWithToken "PUT" $ ImageFlagR "a" "b"
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
  describe "checks CSRF protection" $ do
    forM_ ["PUT", "DELETE"] $ \method -> do
      it ("rejects a " ++ show method ++ " without a token") $ do
        loginAndCheckFlagged
        performMethod method $ ImageFlagR "a" "b"
        statusIs 403
      it ("rejects a form overriding the method to " ++ show method ++
          " without a token") $ do
        loginAndCheckFlagged
        postForm method Nothing
        statusIs 403
    it "accepts a form overriding the method to PUT with a token" $ do
      loginAndCheckFlagged
      pageCsrfToken >>= postForm "PUT" . Just
      statusIs 404
    it "accepts a form overriding the method to DELETE with a token" $ do
      loginAndCheckFlagged
      pageCsrfToken >>= postForm "DELETE" . Just
      followRedirectOK
      bodyContains "Image was not flagged!"
    -- The viewer relies on these two, to tell the user to reload the page
    -- only when the token is the problem.
    it "rejects a JSON request with a stale token as forbidden" $ do
      loginAndCheckFlagged
      request $ do
        setMethod "PUT"
        setUrl $ ImageFlagR "a" "b"
        addRequestHeader (hAccept, "application/json")
        addCsrfHeader "stale"
      statusIs 403
    it "reports an anonymous JSON request as unauthenticated" $ do
      get $ AuthR LoginR
      token <- pageCsrfToken
      request $ do
        setMethod "PUT"
        setUrl $ ImageFlagR "a" "b"
        addRequestHeader (hAccept, "application/json")
        addCsrfHeader token
      statusIs 401
