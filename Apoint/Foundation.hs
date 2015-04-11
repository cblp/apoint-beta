module Foundation where

import Prelude

import Control.Applicative ((<$>))
import Control.Lens ((&))
import Data.Text as Text (Text, empty)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Database.Persist.Sql (SqlPersistT)
import Network.HTTP.Client.Conduit (Manager, HasHttpManager (getHttpManager))
import Network.Mail.Mime ( Address(Address), Encoding(None), Mail(..), Part(..), emptyMail, renderSendMail )
import qualified Database.Persist
import Text.Blaze.Html.Renderer.Utf8 (renderHtml)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Text.Shakespeare.Text (stext)
import Yesod
import Yesod.Auth
import Yesod.Auth.Email
import Yesod.Core.Types (Logger)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Form.Jquery
import Yesod.Static

import Model
import qualified Settings
import Settings (widgetFile, Extra (..))
import Settings.Development (development)
import Settings.StaticFiles


(<$$>) ::
    (Functor f, Functor g) =>
    (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
{-# INLINE (<$$>) #-}


-- | Relation from one note to another
data Rel = RelFrom | RelTo
    deriving (Eq, Read, Show)
instance PathPiece Rel where
    toPathPiece RelFrom = "from"
    toPathPiece RelTo   = "to"
    fromPathPiece "from"  = Just RelFrom
    fromPathPiece "to"    = Just RelTo
    fromPathPiece _       = Nothing


-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Logger
    }

instance HasHttpManager App where
    getHttpManager = httpManager

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the linked documentation for an
-- explanation for this split.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)


defaultLayout' :: Text -> Widget -> Handler Html
defaultLayout' searchQuery widget = do
    master <- getYesod
    mmsg <- getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    let copyright = master & settings & appExtra & extraCopyright
                    & preEscapedToMarkup

    maybeUser <- entityVal <$$> maybeAuth

    pc <- widgetToPageContent $ do
        $(combineStylesheets 'StaticR
            [ css_apoint_css
            , css_normalize_css
            , css_bootstrap_css
            ])
        $(combineScripts 'StaticR
            [ js_jquery_min_js
            , js_jquery_ui_min_js
            , js_bootstrap_min_js
            ])
        $(widgetFile "default-layout")
    giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout = defaultLayout' Text.empty

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

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

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = NotesR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing ->
                Just <$> insert User
                    { userEmail = credsIdent creds
                    , userPassword = Nothing
                    , userVerkey = Nothing
                    , userVerified = False
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [authEmail]

    authHttpManager = httpManager


-- Here's all of the email-specific code
instance YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False

    sendVerifyEmail email _ verurl =
        liftIO $ renderSendMail (emptyMail $ Address Nothing "noreply")
            { mailTo = [Address Nothing email]
            , mailHeaders =
                [ ("Subject", "Verify your email address")
                ]
            , mailParts = [[textPart, htmlPart]]
            }
        where
            textPart = Part
                { partType = "text/plain; charset=utf-8"
                , partEncoding = None
                , partFilename = Nothing
                , partContent = Data.Text.Lazy.Encoding.encodeUtf8
                    [stext|
                        Please confirm your email address by clicking on the link below.

                        #{verurl}

                        Thank you
                    |]
                , partHeaders = []
                }
            htmlPart = Part
                { partType = "text/html; charset=utf-8"
                , partEncoding = None
                , partFilename = Nothing
                , partContent = renderHtml
                    [shamlet|
                        <p>Please confirm your email address by clicking on the link below.
                        <p>
                            <a href=#{verurl}>#{verurl}
                        <p>Thank you
                    |]
                , partHeaders = []
                }

    getVerifyKey uid = runDB $ do
        mu <- get uid
        return $ mu >>= userVerkey

    setVerifyKey uid key = runDB $
        update uid [UserVerkey =. Just key]

    verifyAccount uid = runDB $ do
        mu <- get uid
        case mu of
            Nothing -> return Nothing
            Just _ -> do
                update uid [UserVerified =. True]
                return $ Just uid

    getPassword uid = runDB $ do
        mu <- get uid
        return $ mu >>= userPassword

    setPassword uid pass = runDB $
        update uid [UserPassword =. Just pass]

    getEmailCreds address = runDB $ do
        memail <- getBy $ UniqueUser address
        case memail of
            Nothing -> return Nothing
            Just (Entity userId user) ->
                return $ Just EmailCreds
                    { emailCredsId = userId
                    , emailCredsAuthId = Just userId
                    , emailCredsStatus = userVerified user
                    , emailCredsVerkey = userVerkey user
                    , emailCredsEmail = address
                    }

    getEmail uid = runDB $ do
        mu <- get uid
        return $ fmap userEmail mu


-- Tells where to find the jQuery libraries. We'll just use the defaults,
-- which point to the Google CDN.
instance YesodJquery App


-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)
