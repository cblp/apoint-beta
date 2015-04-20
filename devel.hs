{-# LANGUAGE PackageImports #-}

import            Control.Concurrent        ( forkIO, threadDelay )
import            Control.Monad.Loops       ( untilM_ )
import qualified  Network.Wai.Handler.Warp  as Warp
import            Prelude
import            System.Directory          ( doesFileExist, removeFile )
import            System.Exit               ( exitSuccess )

import qualified  "Apoint" Application


main :: IO ()
main = do
    putStrLn "Starting devel application"
    (port, app) <- Application.getApplicationDev
    let warpSettings = Warp.setPort port Warp.defaultSettings

    forkIO $ Warp.runSettings warpSettings app

    threadDelay 100000 `untilM_` doesFileExist "yesod-devel/devel-terminate"

  where
    terminateDevel = exitSuccess
