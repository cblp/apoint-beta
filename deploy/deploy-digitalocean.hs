#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

import Development.Shake          ( (*>), CmdOption(Traced)
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

    deploy *> \_ -> do
        need1 sourcePath
        upload
        install
        restart

  where
    -- parameters
    packageFile = "apoint_0.0.0_amd64.deb"
    server = "45.55.198.147"
    serviceName = "apoint"
    user = "root"

    -- derivatives
    sourcePath = ".." </> packageFile
    uploadPath = tmp </> packageFile

    -- targets
    deploy = "deploy"

    -- actions
    install = remoteSudo ["dpkg", "--install", uploadPath]

    restart = remoteSudo ["service", "restart", serviceName]

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
