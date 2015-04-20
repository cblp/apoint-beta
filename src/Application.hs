{-# OPTIONS_GHC -fno-warn-orphans #-}

module            Application                     ( makeApplication
                                                  , getApplicationDev
                                                  , makeFoundation
                                                  ) where

import            Control.Monad.Logger            ( runLoggingT )
import qualified  Database.Persist                as Persist
import qualified  Database.Persist.Sql            as Sql
import qualified  Network.HTTP.Client.Conduit     as HTTP
import            Network.Wai                     ( Application )
import            Network.Wai.Logger              ( clockDateCacher )
import qualified  Network.Wai.Middleware.RequestLogger
                                                  as RequestLogger
import            Prelude.Extended
import            System.Log.FastLogger           as FastLogger
import            Yesod.Auth                      ( getAuth )
import            Yesod.Core                      ( messageLoggerSource )
import            Yesod.Core.Dispatch             ( defaultMiddlewaresNoLogging
                                                  , mkYesodDispatch
                                                  , toWaiAppPlain
                                                  )
import            Yesod.Core.Types                ( Logger (Logger), loggerSet )
import            Yesod.Default.Config            ( AppConfig
                                                  , DefaultEnv ( Development )
                                                  , appEnv
                                                  , configSettings
                                                  , csParseExtra
                                                  , loadConfig
                                                  , withYamlEnvironment
                                                  )
import            Yesod.Default.Handlers          ( getFaviconR, getRobotsR )
import            Yesod.Default.Main              ( LogFunc, defaultDevelApp )

import            Foundation
import qualified  Model
import qualified  Settings
import            Settings.Development            ( development )
import qualified  Settings.StaticFiles            as StaticFiles

-- Import all relevant handler modules here.
-- Don't forget to add new modules to your cabal file!
import            Handler.Home    ( getHomeR )
import            Handler.Link    ( postLinkCreateR )
import            Handler.Note    ( getNoteEditR
                                  , getNoteNewR
                                  , getNoteNewRelR, postNoteNewRelR
                                  , getNoteR, postNoteR
                                  , getNotesR, postNotesR
                                  , postNoteArchiveR
                                  , postNoteDeleteR
                                  )
import            Handler.Search  ( getSearchR, getSearchSuggestR )

-- This line actually creates our YesodDispatch instance. It is the second half
-- of the call to mkYesodData which occurs in Foundation.hs. Please see the
-- comments there for more details.
mkYesodDispatch "App" resourcesApp

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeApplication :: AppConfig DefaultEnv Settings.Extra -> IO (Application, LogFunc)
makeApplication conf = do
    foundation <- makeFoundation conf

    -- Initialize the logging middleware
    logWare <- RequestLogger.mkRequestLogger def
        { RequestLogger.outputFormat =
              if development
                  then RequestLogger.Detailed True
                  else RequestLogger.Apache RequestLogger.FromSocket
        , RequestLogger.destination =
              RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    -- Create the WAI application and apply middlewares
    app <- toWaiAppPlain foundation
    let logFunc = messageLoggerSource foundation (appLogger foundation)
    return (logWare $ defaultMiddlewaresNoLogging app, logFunc)

-- | Loads up any necessary settings, creates your foundation datatype, and
-- performs some initialization.
makeFoundation :: AppConfig DefaultEnv Settings.Extra -> IO App
makeFoundation conf = do
    manager <- HTTP.newManager
    s <- StaticFiles.staticSite
    dbconf <- withYamlEnvironment "config/sqlite.yml" (appEnv conf)
              Persist.loadConfig >>=
              Persist.applyEnv
    p <- Persist.createPoolConfig (dbconf :: Settings.PersistConf)

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher

    let logger = Yesod.Core.Types.Logger loggerSet' getter
        foundation = App conf s p manager dbconf logger

    -- Perform database migration using our application's logging settings.
    runLoggingT (Persist.runPool dbconf (Sql.runMigration Model.migrateAll) p)
                (messageLoggerSource foundation logger)

    return foundation

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp loader (fmap fst . makeApplication)
  where
    loader = Yesod.Default.Config.loadConfig (configSettings Development)
        { csParseExtra = Settings.parseExtra
        }
