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
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
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
  ) where

import           Database.Persist.Sql  (ConnectionPool, runSqlPool)
import           Import.NoFoundation
import           Text.Hamlet           (hamletFile)
import           Text.Jasmine          (minifym)

#ifdef DEVELOPMENT
import           Yesod.Auth.Dummy
#endif

import           Yesod.Auth.HashDB     (authHashDBWithForm)

import           Yesod.Auth.Message
import           Yesod.Core.Types      (Logger)
import qualified Yesod.Core.Unsafe     as Unsafe
import           Yesod.Default.Util    (addStaticContentExternal)
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), bfs,
                                        renderBootstrap3, withAutofocus)

import qualified Data.Set              as Set
import qualified Data.Text             as Text
import           Indexer
import           Types                 (FolderClass (..), ImageStatus (..))

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

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

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
      Just <$> defaultClientSessionBackend sessionTimeout
        "config/client_session_key.aes"

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

        -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
        (title, parents) <- breadcrumbs

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
          -- Combine all CSS files, in order to eliminate page
          -- jumps/flicker on load as the various style sheets
          -- override each other in sequence, or simply adjust UI
          -- elements style. By having a single style-sheet, it's
          -- guaranteed that rendering adjustment happens in a single
          -- step (and this results for me in clean reloads).
          $(combineStylesheets 'StaticR [ bootstrap_css_bootstrap_css
                                        , tablesorter_css_theme_bootstrap_css
                                        , font_awesome_css_fontawesome_css
                                        , font_awesome_css_fa_regular_css
                                        , font_awesome_css_fa_solid_css
                                        , corydalis_css_basic_css
                                        ])

          $(combineScripts 'StaticR [ jquery_js_jquery_js
                                    , tablesorter_js_jquery_tablesorter_js
                                    , tablesorter_js_widgets_widget_uitheme_js
                                    , tablesorter_js_widgets_widget_filter_js
                                    , bootstrap_js_bootstrap_js
                                    , corydalis_js_tablesorter_uitheme_simple_js
                                    , corydalis_js_tablesorter_config_js
                                    ])

          $(widgetFile "default-layout")
        let inflist = [1..]::[Int]
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

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
    shouldLog app _source level =
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger = return . appLogger

    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  breadcrumb (StaticR _)    = return ("Static route" , Nothing)
  breadcrumb FaviconR       = return ("Favicon"      , Nothing)
  breadcrumb RobotsR        = return ("Robots"       , Nothing)
  breadcrumb (AuthR _)      = return ("Auth"         , Just HomeR)
  breadcrumb HomeR          = return ("Home"         , Nothing)
  breadcrumb CurateR        = return ("Curate"       , Just HomeR)
  breadcrumb ReloadR        = return ("Reload cache" , Just HomeR)
  breadcrumb (FolderR name) = return (name, Just HomeR)
  breadcrumb (ImageR folder image) = return (image,
                                             Just (FolderR folder))
  breadcrumb (ViewR folder image) = return ("Viewer",
                                             Just (ImageR folder image))
  breadcrumb (ImageBytesR _ image) = return ("Bytes of " <> image,
                                                   Nothing)
  breadcrumb (ImageInfoR _ image) = return ("Information for " <> image,
                                                  Nothing)
  breadcrumb RandomImageInfoR = return ("Random image", Nothing)
  breadcrumb (UntrackedR folder untracked) =
    return ("Untracked " <> untracked, Just (FolderR folder))
  breadcrumb (BrowseFoldersR kind) =
    return ("Browsing folders of type " <>
            Text.intercalate ", " (map toPathPiece kind), Just CurateR)
  breadcrumb (BrowseImagesR kind) =
    return ("Showing images of type " <>
            Text.intercalate ", " (map toPathPiece kind), Just CurateR)
  breadcrumb SearchFoldersR = return ("Search folders", Just HomeR)
  breadcrumb SearchImagesR  = return ("Search images", Just HomeR)
  breadcrumb QuickSearchR   = return ("Quick search", Just HomeR)
  breadcrumb SettingsR      = return ("Settings"     , Just HomeR)
  breadcrumb LensStatsR     = return ("Lens statistsics", Just CurateR)
  breadcrumb (LensInfoR image) = return (image, Just LensStatsR)
  breadcrumb (ListItemsR atom) =
    return ("Listing " <> atomTypeDescriptions atom, Just HomeR)

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

    authenticate creds = runDB $ do
      x <- getBy $ UniqueUser ident
      case x of
        Just (Entity uid _) -> return $ Authenticated uid
        Nothing ->
          case credsPlugin creds of
#if DEVELOPMENT
            "dummy"  -> Authenticated <$> insert User
                        { userName = ident
                        , userPassword = Nothing
                        }
#endif
            _ -> return $ UserError InvalidUsernamePass
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

    authHttpManager = getHttpManager

loginForm :: Form (Text, Text)
loginForm = renderBootstrap3 BootstrapBasicForm $ (,)
    <$> areq textField (withAutofocus $ mkField "username" "Username") Nothing
    <*> areq passwordField (mkField "password" "Password") Nothing
  where mkField :: Text -> Text -> FieldSettings a
        mkField name descr =
          let fs = bfs descr in fs { fsName = Just name }

loginWidget :: Route App -> Widget
loginWidget loginRoute = do
  request <- getRequest
  loginMsg <- appLoginMessage . appSettings <$> getYesod
  let mtok = reqToken request
  (formWidget, formEnctype) <- handlerToWidget $ generateFormPost loginForm
  $(whamletFile "templates/login.hamlet")

-- | access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
    muid <- maybeAuthId
    return $ case muid of
        Nothing -> AuthenticationRequired
        Just _  -> Authorized

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
