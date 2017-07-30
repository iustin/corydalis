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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation where

import Import.NoFoundation
import Text.Hamlet                 (hamletFile)
import Text.Jasmine                (minifym)
import Yesod.Core.Types            (Logger)
import Yesod.Default.Util          (addStaticContentExternal)
import qualified Yesod.Core.Unsafe as Unsafe
import Types (FolderClass(..), ImageStatus(..))
import qualified Data.Text as T
import qualified Data.Set as S

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
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
msgValidTypes = S.fromList [msgSuccess, msgInfo, msgWarning, msgDanger]

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
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
          addStylesheet $ StaticR css_bootstrap_css

          addStylesheet $ StaticR css_tablesorter_theme_bootstrap_css
          addStylesheet $ StaticR css_font_awesome_css
          addStylesheet $ StaticR css_basic_css

          addScript $ StaticR js_jquery_js
          addScript $ StaticR js_jquery_metadata_js
          addScript $ StaticR js_jquery_tablesorter_js
          addScript $ StaticR js_tablesorter_widget_uitheme_js
          addScript $ StaticR js_tablesorter_uitheme_simple_js
          addScript $ StaticR js_bootstrap_js
          addScript $ StaticR js_plotly_js

          $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- Routes not requiring authentication.
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _ _ = return Authorized

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
  breadcrumb HomeR          = return ("Home"         , Nothing)
  breadcrumb ReloadR        = return ("Reload cache" , Nothing)
  breadcrumb (FolderR name) = return ("Folder " `T.append` name,
                                      Just HomeR)
  breadcrumb (ImageR folder image) = return ("Image " `T.append` image,
                                             Just (FolderR folder))
  breadcrumb (UntrackedR folder untracked) =
    return ("Untracked " `T.append` untracked, Just (FolderR folder))
  breadcrumb (BrowseFoldersR kind) =
    return ("Browsing folders of type " `T.append`
            T.intercalate ", " (map toPathPiece kind), Just HomeR)
  breadcrumb (BrowseImagesR kind) =
    return ("Showing images of type " `T.append`
            T.intercalate ", " (map toPathPiece kind), Just HomeR)
  breadcrumb TimelineR      = return ("Timeline"     , Just HomeR)
  breadcrumb SettingsR      = return ("Settings"     , Just HomeR)
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
