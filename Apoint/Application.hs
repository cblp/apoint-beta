{-# OPTIONS_GHC -fno-warn-orphans #-}
module Application
    ( makeApplication
    , getApplicationDev
    , makeFoundation
    ) where

import Import
import Settings
import Yesod.Auth
import Yesod.Default.Config
import Yesod.Default.Main
import Yesod.Default.Handlers
import Network.Wai as Wai
import Network.Wai.Middleware.AcceptOverride as Wai
import Network.Wai.Middleware.Autohead as Wai
import Network.Wai.Middleware.Gzip as Wai
import Network.Wai.Middleware.MethodOverride as Wai
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger
    , outputFormat
    , OutputFormat (..)
    , IPAddrSource (..)
    -- TODO(Ubuntu16) , destination
    )
-- TODO(Ubuntu16) import qualified Network.Wai.Middleware.RequestLogger as RequestLogger
import qualified Database.Persist
import Database.Persist.Sql (runMigration)
import Network.HTTP.Conduit (newManager) -- TODO(Ubuntu16) Network.HTTP.Client.Conduit
import Control.Monad.Logger (runLoggingT)
-- TODO(Ubuntu16) import Control.Concurrent (forkIO, threadDelay)
-- TODO(Ubuntu16) import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize, flushLogStr)
-- TODO(Ubuntu16) import Network.Wai.Logger (clockDateCacher)
-- TODO(Ubuntu16) import Yesod.Core.Types (loggerSet, Logger (Logger))

-- TODO(Ubuntu16) REMOVE {
import System.IO (stdout)
import System.Log.FastLogger (mkLogger)
-- }

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import Handler.Home
import Handler.Link
import Handler.Note
import Handler.Search

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Extra -> IO Application -- TODO(Ubuntu16) IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- mkRequestLogger def
        { outputFormat =
            if development
                then Detailed True
                else Apache FromSocket
        -- TODO(Ubuntu16) , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    -- TODO(Ubuntu16) let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app) -- TODO(Ubuntu16) , logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Extra -> IO App
makeFoundation conf = do
    manager <- newManager
                    def -- TODO(Ubuntu16) REMOVE
    s <- staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Database.Persist.loadConfig >>=
              Database.Persist.applyEnv
    p <- Database.Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    -- TODO(Ubuntu16) loggerSet' <- newStdoutLoggerSet defaultBufSize
    -- TODO(Ubuntu16) (getter, updater) <- clockDateCacher

    {- TODO(Ubuntu16)
    -- If the Yesod logger (as opposed to the request logger middleware) is
    -- used less than once a second on average, you may prefer to omit this
    -- thread and use "(updater >> getter)" in place of "getter" below.  That
    -- would update the cache every time it is used, instead of every second.
    let updateLoop = do
            threadDelay 1000000
            updater
            flushLogStr loggerSet'
            updateLoop
    _ <- forkIO updateLoop

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger
    -}

    -- TODO(Ubuntu16) REMOVE {
    logger <- mkLogger True stdout
    let foundation = App conf s p manager dbconf logger
    -- }

    -- Perform database migration using our application's logging settings.
    runLoggingT
        (Database.Persist.runPool dbconf (runMigration migrateAll) p)
        (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader makeApplication -- TODO(Ubuntu16) (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = parseExtra
        }

-- TODO(Ubuntu16) REMOVE
defaultMiddlewaresNoLogging :: Wai.Middleware
defaultMiddlewaresNoLogging = Wai.acceptOverride . Wai.autohead . Wai.gzip def . Wai.methodOverride
