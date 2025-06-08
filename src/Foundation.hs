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
  ) where

import           Data.Type.Equality
import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import           Text.Hamlet          (hamletFile)
import           Text.Jasmine         (minifym)

#ifdef DEVELOPMENT
import           Yesod.Auth.Dummy
#endif

import           Yesod.Auth.HashDB    (authHashDBWithForm)

import           Yesod.Auth.Message
import           Yesod.Core.Types     (Logger)
import qualified Yesod.Core.Unsafe    as Unsafe
import           Yesod.Default.Util   (addStaticContentExternal)

import qualified Data.Map             as Map
import qualified Data.Set             as Set

import           Handler.Cookies
import           Indexer
import           Pics

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    , appContext     :: Context Repository SearchCache
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

data PageStyle
  = PageBasic  -- ^ Only the most basic CSS/JS. Well, with masonry since
               -- Bootstrap 5. Sigh.
  | PageTable  -- ^ A page with tablesorter.
  | PageView   -- ^ A page with our viewer.
  | PagePlot   -- ^ A page with plotly and tableviewer.
  | PageGrid   -- ^ A page with infinitescroll.
  | PageFBox   -- ^ A page with grid view and fancy box.


pageCSSResources :: PageStyle -> Widget
pageCSSResources PageBasic =
  $(combineStylesheets 'StaticR
     [ bootstrap_css_bootstrap_css
     , font_awesome_css_fontawesome_css
     , font_awesome_css_regular_css
     , font_awesome_css_solid_css
     , corydalis_css_basic_css
     ])

pageCSSResources PageTable =
  $(combineStylesheets 'StaticR
     [ bootstrap_css_bootstrap_css
     , tablesorter_css_theme_bootstrap_css
     , font_awesome_css_fontawesome_css
     , font_awesome_css_regular_css
     , font_awesome_css_solid_css
     , corydalis_css_basic_css
     ])

pageCSSResources PageView = pageCSSResources PageBasic
pageCSSResources PagePlot = pageCSSResources PageTable
pageCSSResources PageGrid = pageCSSResources PageBasic
pageCSSResources PageFBox =
  $(combineStylesheets 'StaticR
     [ bootstrap_css_bootstrap_css
     , font_awesome_css_fontawesome_css
     , font_awesome_css_regular_css
     , font_awesome_css_solid_css
     , corydalis_css_basic_css
     , fancybox_css_jquery_fancybox_css
     ])

pageJSResources :: PageStyle -> Widget
pageJSResources PageBasic =
  addScriptAttrs (StaticR corydalis_js_bundle_basic_js) [("type",  "module")]

pageJSResources PageTable =
  $(combineScripts 'StaticR
     [ jquery_js_jquery_js
     , tablesorter_js_jquery_tablesorter_combined_js
     , bootstrap_js_bootstrap_bundle_js
     , corydalis_js_tablesorter_uitheme_simple_js
     , corydalis_js_tablesorter_config_js
     ])

pageJSResources PageView =
  addScriptAttrs (StaticR corydalis_js_viewer_js) [("type",  "module")]

pageJSResources PagePlot =
  addScriptAttrs (StaticR corydalis_js_bundle_plot_js) [("type",  "module")]

pageJSResources PageGrid =
  $(combineScripts 'StaticR
     [ jquery_js_jquery_js
     , masonry_js_masonry_pkgd_js
     , bootstrap_js_bootstrap_bundle_js
     , imagesloaded_js_imagesloaded_pkgd_js
     , infinite_scroll_js_infinite_scroll_pkgd_js
     , corydalis_js_imagegrid_js
     ])

pageJSResources PageFBox =
  $(combineScripts 'StaticR
     [ jquery_js_jquery_js
     , masonry_js_masonry_pkgd_js
     , bootstrap_js_bootstrap_bundle_js
     , imagesloaded_js_imagesloaded_pkgd_js
     , infinite_scroll_js_infinite_scroll_pkgd_js
     , corydalis_js_imagegrid_js
     , fancybox_js_jquery_fancybox_js
     , corydalis_js_fancybox_js
     ])


routeStyle :: Route App -> PageStyle
routeStyle AboutR                 = PageBasic
routeStyle AuthR{}                = PageBasic
routeStyle BrowseFoldersR{}       = PageGrid
routeStyle BrowseImagesR{}        = PageFBox
routeStyle CameraInfoR{}          = PagePlot
routeStyle CameraStatsR           = PagePlot
routeStyle CurateR                = PagePlot
routeStyle FaviconR               = PageBasic
routeStyle FlaggedImagesListR     = PageBasic
routeStyle FlaggedImagesR         = PageTable
routeStyle FolderR{}              = PageTable
routeStyle HomeR                  = PageBasic
routeStyle ImageBytesR{}          = PageBasic
routeStyle ImageFlagR{}           = PageBasic
routeStyle ImageInfoR{}           = PageBasic
routeStyle ImageR{}               = PageBasic
routeStyle LensInfoR{}            = PagePlot
routeStyle LensStatsR             = PagePlot
routeStyle ListFoldersR           = PageTable
routeStyle ListImagesR            = PageTable
routeStyle ListItemsR{}           = PageTable
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
routeStyle StatusErrorsR          = PageTable
routeStyle ViewR{}                = PageView
routeStyle SearchViewR            = PageBasic

viewMode :: Route App -> Maybe ViewMode
viewMode ListFoldersR      = Just $ ViewFolders PresentationList
viewMode ListImagesR       = Just $ ViewImages PresentationList
viewMode BrowseFoldersR {} = Just $ ViewFolders PresentationGrid
viewMode BrowseImagesR {}  = Just $ ViewImages PresentationGrid
viewMode SearchViewR {}    = Just ViewSingleImage
viewMode ViewR {}          = Just ViewSingleImage
viewMode _                 = Nothing

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
      Just <$> defaultClientSessionBackend sessionTimeout clientSessionKeyFile

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = maybeSecureSessions . defaultYesodMiddleware
      where maybeSecureSessions handler = do
              master <- getYesod
              let settings = appSettings master
              (if appSecureSessions settings  ||
                  appHttps settings
                 then sslOnlyMiddleware sessionTimeout
                 else id) handler

    defaultLayout widget = do
        mmsg <- getMessage
        mmsgKind <- lookupSession msgTypeKey
        let mmsgClass = case mmsgKind of
                          Nothing -> msgInfo
                          Just mmsgKind' -> if mmsgKind' `elem` msgValidTypes
                                              then mmsgKind'
                                              else msgInfo
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
          -- Compute and ship the page-specific CSS and JS
          -- resources. In production mode, these will be even
          -- combined, so one CSS and one JS bundle.
          case route of
            Nothing -> return ()
            Just r -> do
              let style = routeStyle r
              pageCSSResources style
              pageJSResources style
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

        is_auth <- isJust <$> maybeAuthId
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

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO app _source level = return $
        appShouldLogAll (appSettings app)
            || level >= appLogLevel (appSettings app)

    makeLogger = return . appLogger

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
    let r = maybe SearchFoldersNoYearR SearchFoldersByYearR $
            Map.lookup name (repoDirs pics) >>= pdYear
    return (name, Just r)
  breadcrumb (SearchFoldersByYearR year) = return (sformat int year, Nothing)
  breadcrumb SearchFoldersNoYearR = return ("?", Nothing)

  breadcrumb (ImageR folder imname) = return (unImageName imname,
                                             Just (FolderR folder))
  breadcrumb (ViewR folder image) = return ("Viewer",
                                             Just (ImageR folder image))
  breadcrumb (ImageBytesR _ image) = return ("Bytes of " <> unImageName image,
                                              Nothing)
  breadcrumb (MovieBytesR _ image) = return ("Movie component of " <> unImageName image,
                                              Nothing)
  breadcrumb (ImageInfoR _ image) = return ("Information for " <> unImageName image,
                                                  Nothing)
  breadcrumb RandomImageInfoR = return ("Random image", Nothing)
  breadcrumb ListFoldersR = return ("Listing folders", Nothing)
  breadcrumb ListImagesR  = return ("Listing images", Nothing)
  breadcrumb (BrowseFoldersR _)  = return ("Search folders",    Nothing)
  breadcrumb (BrowseImagesR _)   = return ("Search images",     Nothing)
  breadcrumb QuickSearchR        = return ("Quick search",      Nothing)
  breadcrumb SearchR             = return ("Search redirector", Nothing)
  breadcrumb SearchViewR         = return ("View images by search", Nothing)
  breadcrumb SettingsR           = return ("Settings",          Nothing)
  breadcrumb LensStatsR          = return ("Lens statistics",   Just CurateR)
  breadcrumb CameraStatsR        = return ("Camera statistics", Just CurateR)
  breadcrumb (LensInfoR image)   = return (image,               Just LensStatsR)
  breadcrumb (CameraInfoR image) = return (image,               Just CameraStatsR)
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
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
      x <- getBy $ UniqueUser ident
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing ->
#if DEVELOPMENT
          case credsPlugin creds of
            "dummy"  -> Authenticated <$> insert User
                        { userName = ident
                        , userPassword = Nothing
                        }
            _ -> return $ UserError InvalidUsernamePass
#else
          return $ UserError InvalidUsernamePass
#endif
      where ident = credsIdent creds

    -- Simple HashDB auth and in test/dev dummy auth.
    authPlugins _ = authHashDBWithForm loginWidget (Just . UniqueUser):extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins =
#if DEVELOPMENT
                [authDummy]
#else
                []
#endif

    --authHttpManager = getHttpManager

loginWidget :: Route App -> Widget
loginWidget loginRoute = do
  request <- getRequest
  loginMsg <- appLoginMessage . appSettings <$> getYesod
  let mtok = reqToken request
  $(whamletFile "templates/login.hamlet")

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
repoStatusIcon RepoError {}     = "fa-solid fa-exclamation"

instance YesodAuthPersist App

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
