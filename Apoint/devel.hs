{-# LANGUAGE PackageImports #-}

import "Apoint" Application (getApplicationDev)

import Control.Concurrent (forkIO, threadDelay)
import Network.Wai.Handler.Warp
    ( runSettings
    , defaultSettings
    -- , setPort -- TODO(Ubuntu16)
    , settingsPort -- TODO(Ubuntu16) REMOVE
    )
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings (setPort port defaultSettings) app
    loop

    where
        setPort port settings = settings{settingsPort = port}


loop :: IO ()
loop = do
  threadDelay 100000
  e <- doesFileExist "yesod-devel/devel-terminate"
  if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
