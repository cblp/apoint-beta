{-# LANGUAGE PackageImports #-}

import            Control.Concurrent        ( forkIO, threadDelay )
import qualified  Network.Wai.Handler.Warp  as Warp
import            System.Directory          ( doesFileExist, removeFile )
import            System.Exit               ( exitSuccess )

import "Apoint"   Application               ( getApplicationDev )

main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- getApplicationDev
    let warpSettings = Warp.setPort port Warp.defaultSettings
    forkIO $ Warp.runSettings warpSettings app
    loop

loop :: IO ()
loop = do
    threadDelay 100000
    e <- doesFileExist "yesod-devel/devel-terminate"
    if e then terminateDevel else loop

terminateDevel :: IO ()
terminateDevel = exitSuccess
