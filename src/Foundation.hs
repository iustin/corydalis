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

{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation
  ( App(..)
  , Handler
  , Widget
  , Route(..)
  , resourcesApp
  , msgTypeKey
  , msgSuccess
  , msgWarning
  , unsafeHandler
  , getConfig
  , getPics
  , getContext
  , getLastViewMode
  , clientSessionKeyFile
  , isWithinRoot
  , setSameOriginUltDestReferer
  , alwaysRequiresAuthentication
  , hasAccessToStoragePaths
  , hstsHeaderValue
  , makeDummyLoginUser
  , loginTimeKey
  , isLoginCurrent
  , isGenerationCurrent
  ) where

import           Data.Time            (NominalDiffTime, diffUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
import           Data.Type.Equality
import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import           Text.Hamlet          (hamletFile)
import           Text.Jasmine         (minifym)

#ifdef DEVELOPMENT
import           Yesod.Auth.Dummy
#endif

import           Yesod.Auth.HashDB    (authHashDBWithForm, setPassword,
                                       validatePass)
import           Yesod.Auth.Util.PasswordStore (exportSalt, genSaltIO)

import           Yesod.Auth.Message
import           Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe    as Unsafe
import           Yesod.EmbeddedStatic (EmbeddedStatic, embedStaticContent)

import qualified Data.Map.Strict      as Map
import qualified Data.Set             as Set
import qualified Data.Text.Short      as TS


import           Handler.Cookies
import           Indexer
import           Repository
import           Repository.Scan
import           Repository.State
import           Utils.Logging        (sanitizeLogStr)
import           Utils.Session        (requestNewCsrfToken,
                                       rotatingCsrfSessions)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: EmbeddedStatic -- ^ The static files' subsite.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appContext     :: Context Repository SearchCache
    , appDummyLoginUser :: User -- ^ See 'makeDummyLoginUser'.
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerT App IO
-- type Widget = WidgetT App IO ()
mkYesodData "App" $(parseRoutesFile "config/routes")

-- | Message type key
msgTypeKey :: Text
msgTypeKey = "_MSG_TYPE"

-- | Success message.
msgSuccess :: Text
msgSuccess = "success"

-- | Info message.
msgInfo :: Text
msgInfo = "info"

-- | Warning message.
msgWarning :: Text
msgWarning = "warning"

-- | Danger message.
msgDanger :: Text
msgDanger = "danger"

-- | The list of valid message types (identical to Bootstrap alert
-- classes).
msgValidTypes :: Set Text
msgValidTypes = Set.fromList [msgSuccess, msgInfo, msgWarning, msgDanger]

sessionTimeout :: Int
sessionTimeout = 120

-- | The HSTS header's value: browsers remember to only use HTTPS for a
-- year (the usual value), for the subdomains too.
hstsHeaderValue :: ByteString
hstsHeaderValue =
  "max-age=" <> encodeUtf8 (tshow (365 * 24 * 3600 :: Int)) <> "; includeSubDomains"

data PageStyle
  = PageBasic  -- ^ Only the most basic CSS/JS. Well, with masonry since
               -- Bootstrap 5. Sigh. Also sortable tables, which are small.
  | PageView   -- ^ A page with our viewer.
  | PagePlot   -- ^ A page with plotly and tableviewer.
  | PageGrid   -- ^ A page with infinitescroll.

-- | The style sheet of all the pages: Bootstrap, Font Awesome and ours,
-- bundled by esbuild (see @css/bundle.css@).
pageCSSResources :: Widget
pageCSSResources = addStylesheet $ StaticR css_bundle_css

pageJSResources :: PageStyle -> Route EmbeddedStatic
pageJSResources PageBasic = js_bundle_basic_js
pageJSResources PageView  = js_viewer_js
pageJSResources PagePlot  = js_bundle_plot_js
pageJSResources PageGrid  = js_imagegrid_js

routeStyle :: Route App -> PageStyle
routeStyle AboutR                 = PageBasic
routeStyle AuthR{}                = PageBasic
routeStyle BrowseImagesR{}        = PageGrid
routeStyle CameraInfoR{}          = PagePlot
routeStyle CameraStatsR           = PagePlot
routeStyle CurateR                = PagePlot
routeStyle FaviconR               = PageBasic
routeStyle FlaggedImagesListR     = PageBasic
routeStyle FlaggedImagesR         = PageBasic
routeStyle FolderR{}              = PageBasic
routeStyle HomeR                  = PageBasic
routeStyle ImageBytesR{}          = PageBasic
routeStyle ImageFlagR{}           = PageBasic
routeStyle ImageInfoR{}           = PageBasic
routeStyle ImageR{}               = PageBasic
routeStyle LensInfoR{}            = PagePlot
routeStyle LensStatsR             = PagePlot
routeStyle ListFoldersR           = PageBasic
routeStyle ListImagesR            = PageBasic
routeStyle ListItemsR{}           = PageBasic
routeStyle MovieBytesR{}          = PageBasic
routeStyle QuickSearchR           = PageBasic
routeStyle RandomImageInfoR       = PageBasic
routeStyle ReloadR                = PageBasic
routeStyle RobotsR                = PageBasic
routeStyle SearchFoldersByYearR{} = PageBasic
routeStyle SearchFoldersNoYearR   = PageBasic
routeStyle SearchR                = PageBasic
routeStyle SettingsR              = PageBasic
routeStyle StaticR{}              = PageBasic
routeStyle StatusR                = PageBasic
routeStyle StatusErrorsR          = PageBasic
routeStyle ViewR{}                = PageView
routeStyle SearchViewR            = PageBasic

viewMode :: Route App -> Maybe ViewMode
viewMode ListFoldersR     = Nothing
viewMode ListImagesR      = Just ViewImagesList
viewMode BrowseImagesR {} = Just ViewImagesGrid
viewMode SearchViewR {}   = Just ViewSingleImage
viewMode ViewR {}         = Just ViewSingleImage
viewMode _                = Nothing

getLastViewMode :: Handler (Maybe ViewMode)
getLastViewMode = do
  cookie <- lookupCookie viewCookieName
  return $ cookie >>= parseViewMode

setViewMode :: Bool -> ViewMode -> Handler ()
setViewMode secureCookies vm =
  setCookie $ lastViewCookie secureCookies vm

clientSessionKeyFile :: FilePath
clientSessionKeyFile = "config/client_session_key.aes"

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        fromMaybe (getApprootText guessApproot app req) (appRoot $ appSettings app)

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend app =
      (if appSecureSessions $ appSettings app
        then sslOnlySessions
        else id) $
      laxSameSiteSessions $
      rotatingCsrfSessions $
      Just <$> defaultClientSessionBackend sessionTimeout clientSessionKeyFile

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- The defaultCsrfCheckMiddleware validates that incoming write requests
    -- include the session's CSRF token in either a header or POST parameter.
    -- Forms embed the token in a hidden field, and JavaScript reads it from
    -- the csrf-token meta tag in the page layout; no CSRF cookie is set.
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware . defaultCsrfCheckMiddleware

    defaultLayout widget = do
        mmsg <- getMessage
        mmsgKind <- lookupSession msgTypeKey
        let mmsgClass = case mmsgKind of
                          Nothing -> msgInfo
                          Just mmsgKind' -> if mmsgKind' `elem` msgValidTypes
                                              then mmsgKind'
                                              else msgInfo
        is_auth <- isJust <$> maybeAuthId
        ctx <- getContext
        progress <- liftIO $ getProgress ctx
        let scanProgress = pgTotal progress
            scanGoal = pgGoal progress
        repo <- getPics
        let repoState = repoStatus repo
            scanPercent = case repoState of
                            RepoScanning {} -> truncate . (100 *) <$> pgProgress progress
                            _ -> Nothing::Maybe Int

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- layout-wrapper is the entire page. Since the final value
        -- passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        secureCookies <- appSecureSessions . appSettings <$> getYesod
        route <- getCurrentRoute
        forM_ (route >>= viewMode) (setViewMode secureCookies)

        pc <- widgetToPageContent $ do
          -- Ship the CSS bundle and the page-specific JS bundle.
          case route of
            Nothing -> return ()
            Just r -> do
              let style = routeStyle r
                  js_entrypoint = StaticR $ pageJSResources style
              pageCSSResources
              addScriptAttrs js_entrypoint [("type", "module")]
          $(widgetFile "default-layout")

        let inflist = [1..]::[Int]
            isViewer = case route of
                Just (ViewR _ _) -> True
                _                -> False
            crumbsVisibilityClasses =
              -- For viewer, where the structure is year -> folder ->
              -- image -> View, we want to first (at small sizes) show
              -- just the folder, since it has the most meaning, and later
              -- show all.
              if isViewer then "d-lg-block":"d-sm-block":repeat "d-md-block"
              else repeat "d-sm-block"::[Text]
            currentItemVisibilityClass =
              if isViewer then "" else "d-sm-block"::Text
            parentsIdx = zip3 inflist parents crumbsVisibilityClasses

        mtok <- reqToken <$> getRequest
        withUrlRenderer $(hamletFile "templates/layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authentication.
    isAuthorized (AuthR _) _   = return Authorized
    isAuthorized FaviconR _    = return Authorized
    isAuthorized RobotsR _     = return Authorized
    isAuthorized (StaticR _) _ = return Authorized

    -- Reload route always requires authentication.
    isAuthorized ReloadR _     = isAuthenticated
    -- As do some read-only pages, even on public sites.
    isAuthorized route _
      | alwaysRequiresAuthentication route = isAuthenticated
    -- And any other handlers that get a "write" action.
    isAuthorized _       True  = isAuthenticated

    -- Whether all other read-only routes require authentication or
    -- not depends on the build flag. In any case, all authenticated
    -- users are authorized.
    isAuthorized _       False =
#ifdef PUBLIC_SITE
      return Authorized
#else
      isAuthenticated
#endif

    -- Style sheets and scripts generated by widgets (none currently)
    -- are kept in memory by the static subsite (until the server
    -- stops), and served from it, named based on a hash of their
    -- content. This allows expiration dates to be set far in the
    -- future without worry of users receiving stale content.
    addStaticContent = embedStaticContent appStatic StaticR minifym

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level = return $
        appShouldLogAll (appSettings app)
            || level >= appLogLevel (appSettings app)

    makeLogger = return . appLogger

    -- Escapes the messages (see "Utils.Logging"); otherwise as the
    -- default.
    messageLoggerSource app logger loc source level =
      defaultMessageLoggerSource (shouldLogIO app) logger loc source level
      . sanitizeLogStr

    -- Provide proper Bootstrap styling for default displays, like
    -- error pages.
    defaultMessageWidget title body = $(widgetFile "message-widget")

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb (StaticR _)    = return ("Static route" , Nothing)
  breadcrumb FaviconR       = return ("Favicon"      , Nothing)
  breadcrumb RobotsR        = return ("Robots"       , Nothing)
  breadcrumb (AuthR _)      = return ("Auth"         , Nothing)
  breadcrumb HomeR          = return ("Home"         , Nothing)
  breadcrumb CurateR        = return ("Curate"       , Nothing)
  breadcrumb ReloadR        = return ("Reload cache" , Nothing)
  breadcrumb StatusR        = return ("Status"       , Nothing)
  breadcrumb StatusErrorsR  = return ("Errors"       , Just StatusR)
  breadcrumb (FolderR name) = do
    pics <- getPics
    let r = maybe SearchFoldersNoYearR (SearchFoldersByYearR . sformat int) $
            Map.lookup name (repoDirs pics) >>= pdYear
    return (TS.toText name, Just r)
  breadcrumb (SearchFoldersByYearR year) = return (year, Nothing)
  breadcrumb SearchFoldersNoYearR = return ("?", Nothing)

  breadcrumb (ImageR folder imname) = return (TS.toText $ unImageName imname,
                                             Just (FolderR folder))
  breadcrumb (ViewR folder image) = return ("Viewer",
                                             Just (ImageR folder image))
  breadcrumb (ImageBytesR _ image) = return ("Bytes of " <> TS.toText (unImageName image),
                                              Nothing)
  breadcrumb (MovieBytesR _ image) = return ("Movie component of " <> TS.toText (unImageName image),
                                              Nothing)
  breadcrumb (ImageInfoR _ image) = return ("Information for " <> TS.toText (unImageName image),
                                                  Nothing)
  breadcrumb RandomImageInfoR = return ("Random image", Nothing)
  breadcrumb ListFoldersR = return ("Listing folders", Nothing)
  breadcrumb ListImagesR  = return ("Listing images", Nothing)
  breadcrumb (BrowseImagesR _)   = return ("Search images",     Nothing)
  breadcrumb QuickSearchR        = return ("Quick search",      Nothing)
  breadcrumb SearchR             = return ("Search redirector", Nothing)
  breadcrumb SearchViewR         = return ("View images by search", Nothing)
  breadcrumb SettingsR           = return ("Settings",          Nothing)
  breadcrumb LensStatsR          = return ("Lens statistics",   Just CurateR)
  breadcrumb CameraStatsR        = return ("Camera statistics", Just CurateR)
  breadcrumb (LensInfoR image)   = return (TS.toText image,     Just LensStatsR)
  breadcrumb (CameraInfoR image) = return (TS.toText image,     Just CameraStatsR)
  breadcrumb (ListItemsR atom)   =
    return ("Listing " <> atomTypeDescriptions atom, Nothing)
  breadcrumb FlaggedImagesR = return ("Flagged images" , Just CurateR)
  breadcrumb FlaggedImagesListR = return ("Flagged images list" , Just FlaggedImagesR)
  breadcrumb (ImageFlagR folder image) = return ("Update image flag", Just (ImageR folder image))
  breadcrumb AboutR = return ("About", Nothing)

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest _ = AuthR LoginR
    -- Don't let yesod-auth override the above two destinations with any
    -- Referer: header, as that can point to another site (open redirect).
    -- Instead, the login page does this for same-origin referers only.
    redirectToReferer _ = False

    loginHandler = do
      liftHandler setSameOriginUltDestReferer
      defaultLoginHandler

    -- Records the login time, to limit the age of the session (see
    -- 'checkLoginAge'), and the user's session generation (see
    -- 'getAuthEntity'), and replaces the CSRF token, which could have
    -- been known before login (see "Utils.Session"); otherwise as the
    -- default.
    onLogin = do
      requestNewCsrfToken
      now <- liftIO getCurrentTime
      setSession loginTimeKey . tshow $
        (floor (utcTimeToPOSIXSeconds now) :: Int64)
      -- The user id is read from the session directly: going through
      -- maybeAuthId would also check the session generation, which is
      -- not set yet. Without a user (which shouldn't happen right after
      -- logging in), no generation is recorded, which only the initial
      -- generation accepts (see 'isGenerationCurrent').
      muid <- (>>= fromPathPiece) <$> lookupSession credsKey
      muser <- case muid of
        Just uid -> liftHandler . runDB $ get uid
        Nothing  -> return Nothing
      case muser of
        Just user -> setSession loginGenerationKey . tshow $ userGeneration user
        Nothing   -> return ()
      addMessageI "success" NowLoggedIn

    onLogout = do
      requestNewCsrfToken
      deleteSession loginTimeKey
      deleteSession loginGenerationKey

    maybeAuthId = do
      current <- liftHandler checkLoginAge
      if current
        then defaultMaybeAuthId
        else return Nothing

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
      x <- getBy $ UniqueUser ident
      case x of
        Just (Entity uid user)
          | userDeleted user -> return $ UserError InvalidUsernamePass
          | otherwise        -> return $ Authenticated uid
        Nothing ->
#if DEVELOPMENT
          case credsPlugin creds of
            "dummy"  -> Authenticated <$> insert User
                        { userName = ident
                        , userPassword = Nothing
                        , userDeleted = False
                        , userGeneration = 0
                        }
            _ -> return $ UserError InvalidUsernamePass
#else
          return $ UserError InvalidUsernamePass
#endif
      where ident = credsIdent creds

    -- Simple HashDB auth and in test/dev dummy auth.
    authPlugins _ = authHashDBPlugin:extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins =
#if DEVELOPMENT
                [authDummy]
#else
                []
#endif

    --authHttpManager = getHttpManager

-- | The session key holding the login time, in seconds since the epoch.
loginTimeKey :: Text
loginTimeKey = "_LOGIN_TIME"

-- | The session key holding the user's session generation at login.
loginGenerationKey :: Text
loginGenerationKey = "_LOGIN_GEN"

-- | Whether the session generation stored in the session is the user's
-- current one.
--
-- Sessions without a generation, from before it was recorded, are
-- treated as having the initial one (0), so that the upgrade doesn't
-- log out everyone; invalid values are not current.
isGenerationCurrent :: Int        -- ^ The user's current generation.
                    -> Maybe Text -- ^ The user's generation at login, as
                                  -- stored in the session.
                    -> Bool
isGenerationCurrent current loginGeneration =
  maybe (Just 0) readMay loginGeneration == Just current

-- | Whether a login, at the time stored in the session, is still valid.
--
-- Sessions without a (valid) login time, e.g. from before the time was
-- recorded, are not.
isLoginCurrent :: NominalDiffTime  -- ^ The maximum age of a login.
               -> UTCTime          -- ^ The current time.
               -> Maybe Text       -- ^ The login time, as stored.
               -> Bool
isLoginCurrent maxAge now stored =
  case stored >>= readMay of
    Just secs ->
      let loginTime = posixSecondsToUTCTime $ fromIntegral (secs :: Int64)
      in diffUTCTime now loginTime < maxAge
    Nothing -> False

-- | Checks that the login, if any, is not older than the configured
-- maximum age, independently of how active the session was.
--
-- Logs out (and tells the user) otherwise, returning false.
checkLoginAge :: Handler Bool
checkLoginAge = do
  loggedIn <- isJust <$> lookupSession credsKey
  if not loggedIn
    then return True
    else do
      maxAge <- appSessionMaxAge . appSettings <$> getYesod
      now <- liftIO getCurrentTime
      current <- isLoginCurrent maxAge now <$> lookupSession loginTimeKey
      unless current $ do
        deleteSession credsKey
        deleteSession loginTimeKey
        deleteSession loginGenerationKey
        setMessage "Your login has expired, please log in again."
        setSession msgTypeKey msgWarning
      return current

-- | Whether an URL points within the application root.
--
-- The root must be given without a trailing slash; anything else in the
-- URL past the root must start with a slash, so that e.g.
-- @http://host.evil.example@ doesn't match @http://host@.
isWithinRoot :: Text -> Text -> Bool
isWithinRoot root url = url == root || (root <> "/") `isPrefixOf` url

-- | Sets the destination after login to the referring page, if it is part
-- of this site, and no destination is already set.
setSameOriginUltDestReferer :: Handler ()
setSameOriginUltDestReferer = do
  referer <- lookupHeader "Referer"
  render <- getUrlRender
  let home = render HomeR
      root = fromMaybe home $ stripSuffix "/" home
  -- setUltDestReferer re-reads the same header.
  when (maybe False (isWithinRoot root . decodeUtf8) referer)
    setUltDestReferer

-- | The username and password authentication plugin.
--
-- This is the yesod-auth-hashdb plugin, but with a separate login
-- handler: the plugin checks the password, which is slow by design, only
-- for existing users, so the response time would show whether a user
-- exists. Unlike the plugin, this only accepts form data, not JSON.
--
-- This was reported upstream as
-- <https://github.com/paul-rouse/yesod-auth-hashdb/issues/11>; once
-- fixed there, this can go back to using the plugin as is.
authHashDBPlugin :: AuthPlugin App
authHashDBPlugin = plugin { apDispatch = dispatch }
  where plugin = authHashDBWithForm loginWidget (Just . UniqueUser)
        dispatch "POST" ["login"] = postHashDBLoginR >>= sendResponse
        dispatch verb pieces      = apDispatch plugin verb pieces

-- | Handles the login form, as the yesod-auth-hashdb plugin does.
postHashDBLoginR :: AuthHandler App TypedContent
postHashDBLoginR = do
  (mname, mpasswd) <- liftHandler . runInputPost $
    (,) <$> iopt textField "username" <*> iopt textField "password"
  case (mname, mpasswd) of
    (Just name, Just passwd) -> do
      valid <- liftHandler $ checkUserPassword name passwd
      if valid
        then liftHandler . setCredsRedirect $ Creds "hashdb" name []
        else loginErrorMessageI LoginR InvalidUsernamePass
    _ -> loginErrorMessageI LoginR InvalidUsernamePass

-- | Checks a user's password, taking the same time for unknown users.
--
-- For unknown and deleted users, and users without a password, the
-- password is checked against the dummy user's one (see
-- 'makeDummyLoginUser'), but the result is ignored.
checkUserPassword :: Text -> Text -> Handler Bool
checkUserPassword name passwd = do
  muser <- runDB . getBy $ UniqueUser name
  case entityVal <$> muser of
    Just user | not (userDeleted user), isJust (userPassword user) ->
      return $ validatePass user passwd == Just True
    _ -> do
      dummy <- appDummyLoginUser <$> getYesod
      -- Force checking the password, as the result isn't used.
      _ <- evaluate $ validatePass dummy passwd == Just True
      return False

-- | Creates a user with a random password, which is not kept.
--
-- At login, the password of unknown users is checked against this one,
-- so that it takes the same time as for existing users. Its password
-- hash has the default strength, as the ones set by @db-util@.
makeDummyLoginUser :: IO User
makeDummyLoginUser = do
  passwd <- decodeUtf8 . exportSalt <$> genSaltIO
  setPassword passwd User { userName = "", userPassword = Nothing
                          , userDeleted = False, userGeneration = 0 }

loginWidget :: Route App -> Widget
loginWidget loginRoute = do
  request <- getRequest
  loginMsg <- appLoginMessage . appSettings <$> getYesod
  let mtok = reqToken request
  $(whamletFile "templates/login.hamlet")

-- | Read-only routes which require authentication even on public sites.
--
-- The flagged images pages show the names of the users who flagged
-- them, which are also their login names, the settings page shows the
-- server's configuration, e.g. the paths of the picture folders, and
-- the status pages show the scan errors, which can contain such paths
-- as well.
alwaysRequiresAuthentication :: Route App -> Bool
alwaysRequiresAuthentication FlaggedImagesR     = True
alwaysRequiresAuthentication FlaggedImagesListR = True
alwaysRequiresAuthentication SettingsR          = True
alwaysRequiresAuthentication StatusR            = True
alwaysRequiresAuthentication StatusErrorsR      = True
alwaysRequiresAuthentication _                  = False

-- | Whether the user can see where the pictures are stored on the
-- server, i.e. the paths of the configured picture directories, and so
-- the full paths of folders and files.
--
-- Currently, all logged in users can; others only see the paths
-- relative to the configured directories.
hasAccessToStoragePaths :: Handler Bool
hasAccessToStoragePaths = isJust <$> maybeAuthId

-- | access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _  -> Authorized

getContext :: Handler Ctx
getContext = appContext <$> getYesod

getConfig :: Handler Config
getConfig = ctxConfig <$> getContext

-- TODO: replace this with reading from context, after forcing scan at
-- start?
getPics :: Handler Repository
getPics = do
  ctx <- appContext <$> getYesod
  liftIO $ scanAll ctx

repoStatusIcon :: RepoStatus -> Text
repoStatusIcon RepoEmpty        = "fa-solid fa-question"
repoStatusIcon RepoStarting     = "fa-solid fa-hourglass-start"
repoStatusIcon RepoScanning {}  = "fa-solid fa-hourglass-half"
repoStatusIcon RepoRendering {} = "fa-solid fa-check"
repoStatusIcon RepoCleaning {}  = "fa-solid fa-check"
repoStatusIcon RepoFinished {}  = "fa-solid fa-check-double"
repoStatusIcon RepoError {}     = "fa-solid fa-triangle-exclamation"

instance YesodAuthPersist App where
  -- Deleted users don't exist as far as authentication is concerned, so
  -- their sessions are not valid (see 'defaultMaybeAuthId'); nor are
  -- sessions from an older generation of the user (see
  -- 'isGenerationCurrent').
  getAuthEntity uid = liftHandler $ do
    loginGeneration <- lookupSession loginGenerationKey
    muser <- runDB $ get uid
    return $ case muser of
      Just user | not (userDeleted user)
                , isGenerationCurrent (userGeneration user) loginGeneration
                -> Just user
      _ -> Nothing

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
