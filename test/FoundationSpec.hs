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
import           Data.Text.Arbitrary   ()
import           Network.Wai.Test      (simpleHeaders)
import           Test.QuickCheck       (Gen, arbitrary, forAll, (==>))
import           Text.HTML.DOM         (parseLBS)
import           Text.XML.Cursor       (attribute, fromDocument)
import           Web.Cookie
import           Yesod.Auth.HashDB     (defaultStrength, setPassword,
                                        validatePass)
import           Yesod.Auth.Util.PasswordStore (passwordStrength)

import           Handler.Cookies
import           Pics                  (RepoStatus (..), Repository (..),
                                        waitForScan)
import           TestImport

-- | Returns a cookie set by the last response.
responseCookie :: ByteString -> YesodExample App (Maybe SetCookie)
responseCookie name = withResponse $ \response ->
  return $ find ((== name) . setCookieName) $
    [ parseSetCookie value
    | (header, value) <- simpleHeaders response, header == "Set-Cookie" ]

-- | Returns the redirect target of the last response.
responseLocation :: YesodExample App (Maybe ByteString)
responseLocation = withResponse $ \response ->
  return $ lookup "Location" $ simpleHeaders response

-- | Opens a page with the given referer.
getWithReferer :: Route App -> ByteString -> YesodExample App ()
getWithReferer route referer = request $ do
  setMethod "GET"
  setUrl route
  addRequestHeader ("Referer", referer)

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
    lookup "img-src" directives `shouldBe` Just ["'self'", "data:"]
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

-- | A scan error showing server details, as e.g. an IO exception would.
scanError :: Text
scanError = "/srv/secret-photos/2023: openFile: permission denied"

-- | Sets the repository state to a failed scan.
--
-- This first waits for the initial scan, so that it doesn't overwrite
-- the state.
setScanError :: YesodExample App ()
setScanError = do
  ctx <- appContext <$> getTestYesod
  liftIO $ do
    repo <- waitForScan ctx
    atomically $ writeTVar (ctxRepo ctx) repo { repoStatus = RepoError scanError }

-- | Read-only pages which require authentication even on public sites.
privatePages :: [Route App]
privatePages = [FlaggedImagesR, FlaggedImagesListR, SettingsR, StatusR, StatusErrorsR]

-- | Logs in with the password login form.
passwordLogin :: Text -> Text -> YesodExample App ()
passwordLogin name passwd = do
  get $ AuthR LoginR
  request $ do
    setMethod "POST"
    setUrl $ AuthR $ PluginR "hashdb" ["login"]
    addPostParam "username" name
    addPostParam "password" passwd
    addToken_ "form[action$=/hashdb/login] "

-- | Checks that the last password login failed.
checkLoginFailed :: YesodExample App ()
checkLoginFailed = do
  statusIs 303
  responseLocation >>= liftIO . (`shouldSatisfy` maybe False ("/auth/login" `isSuffixOf`))
  followRedirectOK
  bodyContains "Invalid username/password combination"
  checkRouteIs HomeR 303

spec :: Spec
spec = do
  isWithinRootSpec
  passwordLoginSpec
  authenticationSpec
  appSpec

-- | Checks which pages always require authentication.
--
-- The tests are not built as a public site, where all other read-only
-- pages are public, so this checks the list itself, and that the pages
-- in it are redirected to the login page.
authenticationSpec :: Spec
authenticationSpec = describe "authentication" $ do
  forM_ privatePages $ \route ->
    it ("is always required for " ++ show route) $
      alwaysRequiresAuthentication route `shouldBe` True
  forM_ [HomeR, CurateR, FolderR "a", ViewR "a" "b", AboutR] $ \route ->
    it ("depends on the build for " ++ show route) $
      alwaysRequiresAuthentication route `shouldBe` False
  withApp $ describe "repository status" $ do
    it "is shown to logged in users" $ do
      setScanError
      login
      checkRoute HomeR
      bodyContains "Repository scanning failed"
      bodyContains (unpack scanError)
      bodyContains "fa-triangle-exclamation"
      bodyNotContains "fa-server"
    -- The not found page is rendered for unknown URLs, before checking
    -- for authentication.
    forM_ [("the login page", "/auth/login", 200), ("the not found page", "/no-such-page", 404)] $
      \(name, url, status) ->
        it ("is not shown to anonymous users on " ++ name) $ do
          setScanError
          get (url :: Text)
          statusIs status
          bodyNotContains "Repository scanning failed"
          bodyNotContains (unpack scanError)
          bodyNotContains "fa-triangle-exclamation"
          bodyContains "fa-server"
  withApp $ forM_ privatePages $ \route ->
    it ("redirects an anonymous request for " ++ show route ++ " to login") $ do
      get route
      statusIs 303
      responseLocation >>= liftIO . (`shouldSatisfy` maybe False ("/auth/login" `isSuffixOf`))

isWithinRootSpec :: Spec
isWithinRootSpec = describe "isWithinRoot" $ do
  let root = "http://localhost:3000"
  it "accepts the root itself" $
    isWithinRoot root root `shouldBe` True
  it "accepts pages under the root" $ do
    isWithinRoot root "http://localhost:3000/" `shouldBe` True
    isWithinRoot root "http://localhost:3000/curate?x=1" `shouldBe` True
  it "rejects other sites" $ do
    isWithinRoot root "https://evil.example/" `shouldBe` False
    isWithinRoot root "http://localhost:3000.evil.example/" `shouldBe` False
    isWithinRoot root "http://localhost:3000@evil.example/" `shouldBe` False
    isWithinRoot root "http://localhost:30001/" `shouldBe` False
    isWithinRoot root "" `shouldBe` False
  prop "accepts any root itself" $ forAll anyText $ \r ->
    isWithinRoot r r
  prop "accepts any path under a root" $ forAll anyText $ \r ->
    forAll anyText $ \p -> isWithinRoot r (r <> "/" <> p)
  prop "rejects a root followed by anything but a slash" $
    forAll anyText $ \r -> forAll anyText $ \s ->
      not (null s) && not ("/" `isPrefixOf` s) ==>
        not (isWithinRoot r (r <> s))
  prop "rejects URLs not starting with the root" $
    forAll anyText $ \r -> forAll anyText $ \u ->
      not (r `isPrefixOf` u) ==> not (isWithinRoot r u)
  where anyText = arbitrary :: Gen Text

passwordLoginSpec :: Spec
passwordLoginSpec = describe "password login" $ do
  it "creates a dummy user with a valid password hash, as strong as the default" $ do
    dummy <- makeDummyLoginUser
    passwordStrength . encodeUtf8 <$> userPassword dummy `shouldBe` Just defaultStrength
    validatePass dummy "" `shouldBe` Just False
    validatePass dummy "password" `shouldBe` Just False
  it "creates a dummy user with a different password each time" $ do
    dummy1 <- makeDummyLoginUser
    dummy2 <- makeDummyLoginUser
    userPassword dummy1 `shouldNotBe` userPassword dummy2
  withApp $ do
    let createUserWithPassword = do
          user <- setPassword "secret" User { userName = "user", userPassword = Nothing
                                            , userDeleted = False }
          runDB $ insert_ user
    it "logs in with the right password" $ do
      createUserWithPassword
      passwordLogin "user" "secret"
      statusIs 303
      checkRouteIs HomeR 200
    it "refuses a wrong password" $ do
      createUserWithPassword
      passwordLogin "user" "wrong"
      checkLoginFailed
    it "refuses an unknown user" $ do
      createUserWithPassword
      passwordLogin "unknown" "secret"
      checkLoginFailed
    it "refuses a user without a password" $ do
      _ <- createUser "nopassword"
      passwordLogin "nopassword" ""
      checkLoginFailed
    it "refuses a deleted user" $ do
      createUserWithPassword
      runDB $ updateWhere [UserName ==. "user"] [UserDeleted =. True]
      passwordLogin "user" "secret"
      checkLoginFailed

appSpec :: Spec
appSpec = parallel $ withApp $ do
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
  describe "checks deleted users" $ do
    it "logs out a user deleted after logging in" $ do
      userEntity <- createUser "foo"
      authenticateAs userEntity
      checkRoute HomeR
      runDB $ update (entityKey userEntity) [UserDeleted =. True]
      checkRouteIs HomeR 303
    it "doesn't give a deleted user's session to a new user" $ do
      userEntity <- createUser "foo"
      authenticateAs userEntity
      runDB $ update (entityKey userEntity) [UserDeleted =. True]
      newUser <- createUser "bar"
      liftIO $ entityKey newUser `shouldNotBe` entityKey userEntity
      checkRouteIs HomeR 303
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
  describe "checks the redirect after login" $ do
    it "returns to a referring page of this site" $ do
      userEntity <- createUser "foo"
      getWithReferer (AuthR LoginR) "http://localhost:3000/curate"
      authenticateAs userEntity
      statusIs 303
      location <- responseLocation
      liftIO $ location `shouldBe` Just "http://localhost:3000/curate"
    forM_ [ "https://evil.example/fake-login"
          , "http://localhost:3000.evil.example/fake-login"
          , "http://localhost:3000@evil.example/fake-login"
          ] $ \referer ->
      it ("ignores the referer " ++ show referer) $ do
        userEntity <- createUser "foo"
        getWithReferer (AuthR LoginR) referer
        authenticateAs userEntity
        statusIs 303
        location <- responseLocation
        liftIO $ location `shouldBe` Just "http://localhost:3000/"
    it "ignores the referer when logging out" $ do
      login
      getWithReferer (AuthR LogoutR) "https://evil.example/fake-login"
      request $ do
        setMethod "POST"
        setUrl $ AuthR LogoutR
        addToken
      statusIs 303
      location <- responseLocation
      liftIO $ location `shouldBe` Just "http://localhost:3000/auth/login"
  describe "renders no inline scripts, so that the CSP can forbid them" $
    forM_ htmlPages $ \route ->
      it ("on " ++ show route) $ do
        login
        checkRoute route
        scripts <- htmlQuery "script"
        let inline = filter (null . attribute "src" . fromDocument . parseLBS) scripts
        liftIO $ inline `shouldBe` []
  describe "has a container for notifications (see js/toast.ts)" $
    forM_ htmlPages $ \route ->
      it ("on " ++ show route) $ do
        login
        checkRoute route
        htmlCount "#toast-container" 1
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
