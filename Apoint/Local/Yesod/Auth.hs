{-# LANGUAGE ConstraintKinds #-}

module Local.Yesod.Auth where

import Prelude
import Yesod      ( HandlerT, Yesod
                  , acceptsJson
                  , authRoute
                  , getYesod
                  , notAuthenticated
                  , permissionDenied
                  , redirect
                  , setUltDestCurrent
                  )
import Yesod.Auth ( AuthId, YesodAuthPersist, maybeAuthId )


-- | Similar to 'requireAuthId',
-- but returns "HTTP 401 Unauthorized" if user is not authorized.
requireAuthId' :: YesodAuthPersist master => HandlerT master IO (AuthId master)
requireAuthId' =
    maybeAuthId >>= maybe handleAuthLack return
    where
        handleAuthLack = do
            aj <- acceptsJson
            if aj then notAuthenticated else redirectLogin

        redirectLogin :: Yesod master => HandlerT master IO a
        redirectLogin = do
            y <- getYesod
            setUltDestCurrent
            case authRoute y of
                Just z -> redirect z
                Nothing -> permissionDenied "Please configure authRoute"
