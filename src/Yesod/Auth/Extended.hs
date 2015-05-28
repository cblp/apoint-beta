{-# LANGUAGE ConstraintKinds #-}

module Yesod.Auth.Extended ( requireAuth', requireAuthId' ) where

import Database.Persist ( Entity )
import Prelude
import Yesod            ( HandlerT, Yesod
                        , acceptsJson
                        , authRoute
                        , getYesod
                        , notAuthenticated
                        , permissionDenied
                        , redirect
                        , setUltDestCurrent
                        )
import Yesod.Auth       ( AuthEntity, AuthId, YesodAuthPersist
                        , maybeAuth, maybeAuthId
                        )


-- | Similar to "requireAuth",
-- but returns "HTTP 401 Unauthorized" if user is not authorized.
requireAuth' :: YesodAuthPersist master =>
                HandlerT master IO (Entity (AuthEntity master))
requireAuth' = maybeAuth >>= maybe handleAuthLack return


-- | Similar to "requireAuthId",
-- but returns "HTTP 401 Unauthorized" if user is not authorized.
requireAuthId' :: YesodAuthPersist master => HandlerT master IO (AuthId master)
requireAuthId' = maybeAuthId >>= maybe handleAuthLack return


handleAuthLack :: YesodAuthPersist master => HandlerT master IO a
handleAuthLack = do
    aj <- acceptsJson
    if aj then notAuthenticated else redirectLogin
  where
    redirectLogin = do
        y <- getYesod
        setUltDestCurrent
        case authRoute y of
            Just z -> redirect z
            Nothing -> permissionDenied "Please configure authRoute"
