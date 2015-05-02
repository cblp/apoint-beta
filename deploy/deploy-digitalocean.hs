#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

import Development.Shake          ( (*>)
                                  , command_, shakeArgs, shakeOptions, want
                                  )
import Development.Shake.FilePath ( (</>) )

main :: IO ()
main = shakeArgs shakeOptions $ do
    want [deployed]

    deployed *> \_ -> do
        upload
        install
        restart

  where
    -- parameters
    packageFile = "haskell-apoint-utils_0.0.0_amd64.deb"
    server = "45.55.198.147"
    serviceName = "apoint"
    user = "root"

    -- derivatives
    sourcePath = ".." </> packageFile
    uploadPath = tmp </> packageFile

    -- targets
    deployed = "deployed"

    -- actions
    install = remoteSudo ["dpkg", "--install", uploadPath]

    restart = remoteSudo ["service", "restart", serviceName]

    upload = command_ [] "scp" [sourcePath, remotePath user server uploadPath]

    remoteSudo args = command_ [] "ssh" ((user ++ "@" ++ server) : args)

    -- constants
    tmp = "/tmp"

(<:>) :: FilePath -> FilePath -> FilePath
a <:> b = a ++ ":" ++ b

remotePath :: String -> String -> FilePath -> String
remotePath user server uploadPath = concat [user, "@", server, ":", uploadPath]
