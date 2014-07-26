{-# LANGUAGE  FlexibleInstances
            , TypeSynonymInstances
  #-}

module Access where

import Control.Monad (when)

import Local.Yesod.Auth (requireAuthId')

import Import


data AccessMode = Read | Update | Delete


class UserLocator userLocator where
    getUserId :: userLocator -> Handler UserId

instance UserLocator UserId where
    getUserId = return . id

data CurrentUser = CurrentUser

instance UserLocator CurrentUser where
    getUserId _ = requireAuthId'


class NoteLocator noteLocator where
    getNote :: noteLocator -> Handler Note

instance NoteLocator Note where
    getNote = return . id

instance NoteLocator NoteId where
    getNote = runDB . get404


authorize ::
    (UserLocator userLocator, NoteLocator noteLocator) =>
    AccessMode -> userLocator -> noteLocator -> Handler ()
authorize _ userLoc noteLoc = do
    -- access mode really doesn't matter yet
    userId <- getUserId userLoc
    note <- getNote noteLoc
    when (userId /= noteAuthor note) $
        permissionDenied "You are not the author of this note"
