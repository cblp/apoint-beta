#!/usr/bin/env runhaskell
{-# OPTIONS -Wall -Werror #-}

module Main ( main ) where

import Control.Arrow              ( (>>>) )
-- import Control.Error              ( hoistEither, note, runScript, scriptIO )
-- import Control.Monad              ( forM, forM_ )
import Control.Monad.IO.Class     ( liftIO )
-- import Data.Char                  ( toLower )
import Data.Maybe                 ( fromJust )
import qualified Data.Text        as Text
import Development.Shake          ( (*>)
                                  , Action
                                  , cmd
                                  , need
                                  , shakeArgs
                                  , shakeOptions
                                  , want
                                  )
-- import Development.Shake.FilePath ( (</>), (<.>) )
-- import Distribution.Package       ( PackageId, PackageName (..)
--                                   , pkgName, pkgVersion
--                                   )
-- import Distribution.Text          ( display, simpleParse )
import System.Directory           ( getCurrentDirectory, setCurrentDirectory )

-- Utopic:
--     cmd:
--         cabal install --avoid-reinstalls --dry-run
--             aeson-0.7.0.3
--             binary-0.5.1.1
--             hashtables-1.0.1.8
--             persistent-1.3.1.1
--             persistent-mongoDB
-- packages :: [String]
-- packages =  [ "bson-0.2.2"
--             , "mongoDB-1.4.1.1"
--             , "pool-conduit-0.1.2.3"
--             , "persistent-mongoDB-1.4.3"
--             ]

main :: IO ()
main = shakeArgs shakeOptions $ do
    want [deb_persistent_mongoDB_1_4_3]

    deb_bson_0_2_2 *> build

    deb_persistent_mongoDB_1_4_3 *> \pkg -> do
        need [deb_bson_0_2_2]
        build pkg

  where
    deb_bson_0_2_2                = deb "bson"                "0.2.2"
    deb_persistent_mongoDB_1_4_3  = deb "persistent-mongoDB"  "1.4.3"

    deb name version = "libghc-" ++ name ++ "-dev_" ++ version ++ "_amd64.deb"

    build debname = do
        let (pkg, version) = parseDebFileName debname
        let pkgid = pkg ++ "-" ++ version
        cabal_get pkgid
        withCurrentDirectory pkgid $ do
            cabal_debian
            debuild

    cabal_debian = cmd "cabal-debian" :: Action ()
    cabal_get pkgid = cmd "cabal" "get" pkgid :: Action ()
    debuild = cmd "debuild" "-us" "-uc" :: Action ()

    parseDebFileName :: String -> (String, String)
    parseDebFileName =  Text.pack
                    >>> Text.stripPrefix (Text.pack "libghc-")
                    >>> fromJust
                    >>> Text.stripSuffix (Text.pack "_amd64.deb")
                    >>> fromJust
                    >>> Text.splitOn (Text.pack "-dev_")
                    >>> map Text.unpack
                    >>> listToPair

-- main' :: IO ()
-- main' = runScript $ do
--     pkgids <- forM packages $
--         hoistEither . note "simpleParse" . simpleParse
-- --     forM_ pkgids $ scriptIO . print
--     let debiannames = map (debianName . pkgName) pkgids
--     scriptIO $ do
--         putStrLn "Targets:"
--         forM_ debiannames $ putStrLn . ('\t' :)
--         putStrLn ""
--
--     scriptIO $ shakeArgs shakeOptions $ do
--         want debiannames
--
--         forM_ (zip pkgids debiannames) $ \(pkgid, debianname) -> do
--             let srcDir = display pkgid
--                 srcCabal = srcDir </> display (pkgName pkgid) <.> "cabal"
--                 deb = debFileName pkgid
--
--             srcCabal *> \_ ->
--                 cabal_get pkgid
--
--             debianname *> \_ ->
--                 needed [deb]
--
--             deb *> \_ -> do
--                 needed [srcCabal]
--                 withCurrentDirectory srcDir $ do
--                     cabal_debian
--                     debuild
--   where
--     cabal_debian = cmd "cabal-debian" :: Action ()
--     cabal_get pkgid = cmd "cabal" "get" $ display pkgid :: Action ()
--     debuild = cmd "debuild" "-us" "-uc" :: Action ()

-- debianName :: PackageName -> String
-- debianName (PackageName haskellName) =
--     "libghc-" ++ map toLower haskellName ++ "-dev"

-- debFileName :: PackageId -> FilePath
-- debFileName pkgid =
--     let version = pkgVersion pkgid
--         arch = "amd64"
--     in  concat  [ debianName (pkgName pkgid)
--                 , "_" , display version
--                 , "_" , arch
--                 , ".deb"
--                 ]

withCurrentDirectory :: FilePath -> Action a -> Action a
withCurrentDirectory path action = do
    prev <- liftIO getCurrentDirectory
    liftIO $ setCurrentDirectory path
    result <- action
    liftIO $ setCurrentDirectory prev
    return result

listToPair :: [a] -> (a, a)
listToPair [a, b] = (a, b)
listToPair _ = error "not a pair"
