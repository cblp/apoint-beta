#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

import Control.Monad              ( forM_ )
import Development.Shake          ( (~>), CmdOption(Traced)
                                  , command_
                                  , need
                                  , shakeArgs
                                  , shakeOptions
                                  , want
                                  )
import Development.Shake.FilePath ( (</>) )

main :: IO ()
main = shakeArgs shakeOptions $ do
    want1 deploy

    deploy ~> do
        need1 sourcePath
        upload
        install
        forM_ services $ \s -> do
            restart s
            status s  -- TODO check port

  where
    -- parameters
    packageFile = "apoint_0.0.0_amd64.deb"
    services = ["apoint", "nginx"]
    user = "root"
    server = next02
      where
        next02 = "45.55.198.147"

    -- derivatives
    sourcePath = ".." </> packageFile
    uploadPath = tmp </> packageFile

    -- targets
    deploy = "deploy"

    -- actions
    install = remoteSudo ["dpkg", "--install", uploadPath]

    restart serviceName = remoteSudo ["service", serviceName, "restart"]

    status serviceName = remoteSudo ["service", serviceName, "status"]

    upload =
        let targetPath = concat [user, "@", server, ":", uploadPath]
            trace = unwords ["scp", sourcePath, targetPath]
        in  command_ [Traced trace] "scp" [sourcePath, targetPath]

    remoteSudo args =
        command_ [Traced $ unwords args] "ssh" ((user ++ "@" ++ server) : args)

    -- constants
    tmp = "/tmp"

    -- util
    need1 = need . (:[])
    want1 = want . (:[])
