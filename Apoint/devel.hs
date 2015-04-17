{-# LANGUAGE PackageImports #-}

import          Control.Concurrent        ( forkIO, threadDelay )
import          Network.Wai.Handler.Warp  ( defaultSettings
                                          , runSettings
                                          , setPort
                                          )
import          System.Directory          ( doesFileExist, removeFile )
import          System.Exit               ( exitSuccess )

import "Apoint" Application               ( getApplicationDev )

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    forkIO $ runSettings (setPort port defaultSettings) app
    loop

loop :: IO ()
loop = do
    threadDelay 100000
    e <- doesFileExist "yesod-devel/devel-terminate"
    if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
