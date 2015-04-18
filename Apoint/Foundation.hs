module Foundation where

import            Control.Monad.Logger          ( LogLevel ( LevelWarn )
                                                , logInfo
                                                )
import qualified  Data.Text                     as Text
import            Data.Text                     ( Text )
import            Data.Text.Lazy.Encoding       ( encodeUtf8 )
import qualified  Database.Persist              as Persist
import            Database.Persist              ( (=.)
                                                , Entity (..)
                                                , entityVal
                                                , get
                                                , getBy
                                                , insert
                                                , update
                                                )
import            Database.Persist.Sql          ( SqlPersistT )
import qualified  Network.HTTP.Client.Conduit   as HTTP
import qualified  Network.Mail.Mime             as Mail
import            Prelude.Extended
import            Text.Blaze                    ( preEscapedToMarkup )
import            Text.Blaze.Html               ( Html )
import            Text.Blaze.Html.Renderer.Utf8 ( renderHtml )
import            Text.Hamlet                   ( hamletFile
                                                , shamlet
                                                )
import            Text.Jasmine                  ( minifym )
import            Text.Shakespeare.Text         ( stext )
import            Text.Shakespeare.I18N         ( RenderMessage (..)
                                                , mkMessage
                                                )
import            Web.PathPieces                ( PathPiece (..) )
import qualified  Yesod.Auth                    as YesodAuth
import            Yesod.Auth                    ( Auth
                                                , Route ( LoginR, LogoutR )
                                                )
import qualified  Yesod.Auth.Email              as YesodAuth
import qualified  Yesod                         as Yesod
import            Yesod.Core                    ( Yesod )
import qualified  Yesod.Core.Types              as Yesod
import            Yesod.Core.Types              ( AuthResult ( Authorized )
                                                , PageContent ( pageBody
                                                              , pageHead
                                                              , pageTitle
                                                              )
                                                )
import            Yesod.Default.Config          ( AppConfig, DefaultEnv
                                                , appExtra, appRoot
                                                )
import            Yesod.Default.Util            ( addStaticContentExternal )
import            Yesod.Form                    ( FormMessage
                                                , FormResult
                                                , MForm
                                                , defaultFormMessage
                                                )
import            Yesod.Form.Jquery             ( YesodJquery )
import            Yesod.Persist                 ( YesodPersist ( runDB )
                                                , YesodPersistBackend
                                                , YesodPersistRunner
                                                  ( getDBRunner )
                                                , defaultGetDBRunner
                                                , defaultRunDB
                                                )
import            Yesod.Routes.Parse            ( parseRoutesFile )
import            Yesod.Routes.Class            ( RenderRoute ( renderRoute ) )
import            Yesod.Static                  ( Route (StaticRoute)
                                                , Static
                                                , base64md5
                                                )

import            Model
                  -- ^ TODO explicit import UniqueUser
import qualified  Settings                      ( PersistConf
                                                , staticDir, staticRoot
                                                )
import            Settings                      ( Extra (..), widgetFile )
import            Settings.Development          ( development )
import qualified  Settings.StaticFiles          as StaticFiles


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
    , connPool :: Persist.PersistConfigPool Settings.PersistConf
      -- ^ Database connection pool.
    , httpManager :: HTTP.Manager
    , persistConfig :: Settings.PersistConf
    , appLogger :: Yesod.Logger
    }

instance HTTP.HasHttpManager App where
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
Yesod.mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (Yesod.HandlerT App IO) (FormResult x, Widget)


defaultLayout' :: Text -> Widget -> Handler Html
defaultLayout' searchQuery widget = do
    master <- Yesod.getYesod
    mmsg <- Yesod.getMessage

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    let copyright = master & settings & appExtra & extraCopyright
                    & preEscapedToMarkup

    maybeUser <- fmap entityVal <$> YesodAuth.maybeAuth

    pc <- Yesod.widgetToPageContent $ do
        $(StaticFiles.combineStylesheets 'StaticR
            [ StaticFiles.css_apoint_css
            , StaticFiles.css_normalize_css
            , StaticFiles.css_bootstrap_css
            ])
        $(StaticFiles.combineScripts 'StaticR
            [ StaticFiles.js_jquery_min_js
            , StaticFiles.js_jquery_ui_min_js
            , StaticFiles.js_bootstrap_min_js
            ])
        $(widgetFile "default-layout")
    Yesod.giveUrlRenderer
        $(hamletFile "templates/default-layout-wrapper.hamlet")


-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = Yesod.ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = Just <$> Yesod.defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    defaultLayout = defaultLayout' Text.empty

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $
        uncurry (Yesod.joinPath y (Settings.staticRoot $ settings y)) $
        renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Routes not requiring authenitcation.
    isAuthorized (AuthR _)  _ = return Authorized
    isAuthorized FaviconR   _ = return Authorized
    isAuthorized RobotsR    _ = return Authorized
    -- Default to Authorized for now.
    isAuthorized _          _ = return Authorized

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
    jsLoader _ = Yesod.BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level = development || level >= LevelWarn

    makeLogger = return . appLogger

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

instance YesodAuth.YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = NotesR
    -- Where to send a user after logout
    logoutDest _ = HomeR

    getAuthId creds = runDB $ do
        x <- getBy $ UniqueUser $ YesodAuth.credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Just uid
            Nothing ->
                Just <$> insert User
                    { userEmail = YesodAuth.credsIdent creds
                    , userPassword = Nothing
                    , userVerkey = Nothing
                    , userVerified = False
                    }

    -- You can add other plugins like BrowserID, email or OAuth here
    authPlugins _ = [YesodAuth.authEmail]

    authHttpManager = httpManager


-- Here's all of the email-specific code
instance YesodAuth.YesodAuthEmail App where
    type AuthEmailId App = UserId

    afterPasswordRoute _ = HomeR

    addUnverified email verkey =
        runDB $ insert $ User email Nothing (Just verkey) False

    sendVerifyEmail email _ verurl = do
        $logInfo $ mconcat ["email = ", email, ", verurl = ", verurl]
        liftIO $ Mail.renderSendMail
            (Mail.emptyMail $ Mail.Address Nothing "noreply")
                { Mail.mailTo = [Mail.Address Nothing email]
                , Mail.mailHeaders = [("Subject", "Verify your email address")]
                , Mail.mailParts = [[textPart, htmlPart]]
                }
        where
            textPart = Mail.Part
                { Mail.partType = "text/plain; charset=utf-8"
                , Mail.partEncoding = Mail.None
                , Mail.partFilename = Nothing
                , Mail.partContent = Data.Text.Lazy.Encoding.encodeUtf8
                    [stext|
                        Please confirm your email address by clicking on the link below.

                        #{verurl}

                        Thank you
                    |]
                , Mail.partHeaders = []
                }
            htmlPart = Mail.Part
                { Mail.partType = "text/html; charset=utf-8"
                , Mail.partEncoding = Mail.None
                , Mail.partFilename = Nothing
                , Mail.partContent = renderHtml
                    [shamlet|
                        <p>Please confirm your email address by clicking on the link below.
                        <p>
                            <a href=#{verurl}>#{verurl}
                        <p>Thank you
                    |]
                , Mail.partHeaders = []
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
                return $ Just YesodAuth.EmailCreds
                    { YesodAuth.emailCredsId      = userId
                    , YesodAuth.emailCredsAuthId  = Just userId
                    , YesodAuth.emailCredsStatus  = userVerified user
                    , YesodAuth.emailCredsVerkey  = userVerkey user
                    , YesodAuth.emailCredsEmail   = address
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
getExtra = appExtra . settings <$> Yesod.getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
