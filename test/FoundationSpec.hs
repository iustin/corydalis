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

module FoundationSpec (spec) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Map              as M
import           Network.Wai.Test      (simpleHeaders)
import           Text.HTML.DOM         (parseLBS)
import           Text.XML.Cursor       (attribute, fromDocument)
import           Web.Cookie

import           Handler.Cookies
import           TestImport

-- | Returns a cookie set by the last response.
responseCookie :: ByteString -> YesodExample App (Maybe SetCookie)
responseCookie name = withResponse $ \response ->
  return $ find ((== name) . setCookieName) $
    [ parseSetCookie value
    | (header, value) <- simpleHeaders response, header == "Set-Cookie" ]

-- | Pages that render (with an empty repository) when logged in.
htmlPages :: [Route App]
htmlPages = [ HomeR, CurateR, FlaggedImagesR, CameraStatsR, LensStatsR
            , SettingsR, AboutR, StatusR, ListFoldersR, ListImagesR
            , BrowseImagesR 0, AuthR LoginR
            ]

-- | Returns the directives of the security policy of the last response.
cspDirectives :: YesodExample App [(Text, [Text])]
cspDirectives = withResponse $ \response ->
  case [ value | (header, value) <- simpleHeaders response
               , header == "Content-Security-Policy" ] of
    [value] -> return [ (name, args)
                      | directive <- splitSeq ";" (decodeUtf8 value)
                      , name:args <- [words directive] ]
    values  -> liftIO (expectationFailure $
                 "Expected one Content-Security-Policy header, got: " ++
                 show values) >> return []

-- | Checks that the last response forbids content type sniffing.
responseNoSniff :: YesodExample App ()
responseNoSniff = withResponse $ \response ->
  liftIO $ lookup "X-Content-Type-Options" (simpleHeaders response)
    `shouldBe` Just "nosniff"

-- | Checks the important properties of the security policy.
checkSecurityPolicy :: YesodExample App ()
checkSecurityPolicy = do
  directives <- cspDirectives
  liftIO $ do
    lookup "default-src" directives `shouldBe` Just ["'self'"]
    lookup "script-src" directives `shouldBe` Just ["'self'"]
    lookup "object-src" directives `shouldBe` Just ["'none'"]
    lookup "base-uri" directives `shouldBe` Just ["'self'"]
    lookup "frame-ancestors" directives `shouldBe` Just ["'none'"]

handlerView :: [(Route App, ViewMode)]
handlerView = [ (BrowseImagesR 0, ViewImagesGrid)
              , (ListImagesR, ViewImagesList)
              ]

checkViewCookie :: Maybe ViewMode -> YesodExample App ()
checkViewCookie expected = request $ do
  cookies <- getRequestCookies
  let actual = do
        cookieB <- M.lookup (encodeUtf8 viewCookieName) cookies
        parseViewMode . decodeUtf8 . setCookieValue $ cookieB
  liftIO $ actual `shouldBe` expected

spec :: Spec
spec = parallel $ withApp $ do
  describe "checks view pages set their cookie" $ do
    forM_ handlerView $ \(route, symbol) ->
      it ("validates route " ++ show route) $ do
      login
      checkRoute route
      checkViewCookie $ Just symbol
  it "checks that a normal handler doesn't reset the view mode" $ do
    login
    forM_ [CurateR, LensStatsR] $ \route -> do
      checkRoute route
      checkViewCookie Nothing
  describe "checks the security cookie attributes" $ do
    it "sets the session cookie as SameSite=Lax" $ do
      get $ AuthR LoginR
      cookie <- responseCookie "_SESSION"
      liftIO $ (setCookieSameSite =<< cookie) `shouldBe` Just sameSiteLax
    it "does not set a CSRF cookie" $ do
      get $ AuthR LoginR
      cookie <- responseCookie $ BS8.pack "XSRF-TOKEN"
      liftIO $ isNothing cookie `shouldBe` True
  describe "checks CSRF protection on authentication" $ do
    it "rejects a login without a token" $ do
      userEntity <- createUser "foo"
      get $ AuthR LoginR
      request $ do
        setMethod "POST"
        addPostParam "ident" $ userName $ entityVal userEntity
        setUrl $ AuthR $ PluginR "dummy" []
      statusIs 403
      checkRouteIs HomeR 303
    it "accepts the token from the password login form" $ do
      get $ AuthR LoginR
      request $ do
        setMethod "POST"
        setUrl $ AuthR $ PluginR "hashdb" ["login"]
        addPostParam "username" "foo"
        addPostParam "password" "bar"
        addToken_ "form[action$=/hashdb/login] "
      -- The credentials are wrong, but they got as far as being checked.
      statusIs 303
    it "rejects a password login without a token" $ do
      get $ AuthR LoginR
      request $ do
        setMethod "POST"
        setUrl $ AuthR $ PluginR "hashdb" ["login"]
        addPostParam "username" "foo"
        addPostParam "password" "bar"
      statusIs 403
    it "redirects an anonymous write to login, instead of rejecting it" $ do
      get $ AuthR LoginR
      post ReloadR
      statusIs 303
    it "keeps the token across logging out and in again" $ do
      userEntity <- createUser "foo"
      authenticateAs userEntity
      checkRoute HomeR
      tokenBefore <- pageCsrfToken
      checkRoute $ AuthR LogoutR
      request $ do
        setMethod "POST"
        setUrl $ AuthR LogoutR
        addToken
      authenticateAs userEntity
      checkRoute HomeR
      tokenAfter <- pageCsrfToken
      liftIO $ tokenAfter `shouldBe` tokenBefore
    it "allows logging out via the button in the page layout" $ do
      login
      checkRoute HomeR
      request $ do
        setMethod "POST"
        setUrl $ AuthR LogoutR
        addToken_ "form[action$=/auth/logout] "
      statusIs 303
      checkRouteIs HomeR 303
    it "still allows logging out via the logout page" $ do
      login
      checkRoute $ AuthR LogoutR
      request $ do
        setMethod "POST"
        setUrl $ AuthR LogoutR
        addToken
      statusIs 303
      checkRouteIs HomeR 303
  describe "renders no inline scripts, so that the CSP can forbid them" $
    forM_ htmlPages $ \route ->
      it ("on " ++ show route) $ do
        login
        checkRoute route
        scripts <- htmlQuery "script"
        let inline = filter (null . attribute "src" . fromDocument . parseLBS) scripts
        liftIO $ inline `shouldBe` []
  describe "checks the security policy" $ do
    forM_ htmlPages $ \route ->
      it ("is sent with " ++ show route) $ do
        login
        checkRoute route
        checkSecurityPolicy
    it "is sent to anonymous users" $ do
      get $ AuthR LoginR
      checkSecurityPolicy
    it "is sent with error pages" $ do
      login
      get ("/no-such-page" :: Text)
      statusIs 404
      checkSecurityPolicy
  describe "checks the nosniff header" $ do
    it "is sent with pages" $ do
      login
      checkRoute HomeR
      responseNoSniff
    it "is sent with error pages" $ do
      login
      get ("/no-such-page" :: Text)
      statusIs 404
      responseNoSniff
