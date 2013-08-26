{-# LANGUAGE FlexibleInstances #-}

module Foundation where

import Prelude
import Yesod
--import Yesod.Form.Jquery
import Yesod.Static
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import Settings.StaticFiles
import Settings (widgetFile, Extra (..))
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)
import System.Log.FastLogger (Logger)
import qualified Data.Text as T
import Data.Text (Text)
import Pics (FolderClass(..))

-- | Custom yesod instance for FolderClass. This really could use some TH.
instance PathPiece FolderClass where
  toPathPiece FolderEmpty       = "empty"
  toPathPiece FolderRaw         = "raw"
  toPathPiece FolderStandalone  = "standalone"
  toPathPiece FolderUnprocessed = "unprocessed"
  toPathPiece FolderProcessed   = "processed"
  toPathPiece FolderMixed       = "mixed"
  fromPathPiece "empty"       = Just FolderEmpty
  fromPathPiece "raw"         = Just FolderRaw
  fromPathPiece "standalone"  = Just FolderStandalone
  fromPathPiece "unprocessed" = Just FolderUnprocessed
  fromPathPiece "processed"   = Just FolderProcessed
  fromPathPiece "mixed"       = Just FolderMixed
  fromPathPiece _             = Nothing

-- | Custom Path piece instance for [FolderClass].
instance PathPiece [FolderClass] where
  toPathPiece = T.intercalate "+" . map toPathPiece
  fromPathPiece = mapM fromPathPiece . T.split (=='+')

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , httpManager :: Manager
    , appLogger :: Logger
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm Handler (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        let timeout = 120 * 60 -- 120 minutes
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend key getCachedDate

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        (title, parents) <- breadcrumbs

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        extra <- getExtra
        let add f path = f $ T.pack $ extraJSUrl extra ++ path

        pc <- widgetToPageContent $ do
          add addScriptRemote "jquery/jquery.js"
          add addScriptRemote "jquery-metadata/jquery.metadata.js"
          add addScriptRemote "jquery-tablesorter/jquery.tablesorter.js"
          add addStylesheetRemote "jquery-tablesorter/themes/blue/style.css"

          $(widgetFile "normalize")
          addStylesheet $ StaticR css_basic_css
          $(widgetFile "default-layout")
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifym genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    --getLogger = return . appLogger

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

--instance YesodJquery App

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

instance YesodBreadcrumbs App where
  breadcrumb (StaticR _) = return ("Static route", Nothing)
  breadcrumb FaviconR = return ("Favicon", Nothing)
  breadcrumb RobotsR = return ("Robots", Nothing)
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb UnprocessedR = return ("Unprocessed NEFs", Just HomeR)
  breadcrumb (FolderR name) = return ("Folder " `T.append` name,
                                      Just HomeR)
  breadcrumb (BrowseFoldersR kind) =
    return (T.pack $ "Browsing folders of type " ++ show kind, Just HomeR)
