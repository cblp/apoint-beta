{-# LANGUAGE  FlexibleInstances
            , TypeSynonymInstances
  #-}

module Access where

import Prelude.Extended
import Yesod.Auth.Extended  ( requireAuthId' )
import Yesod.Core           ( permissionDenied )
import Yesod.Persist        ( (==.), entityKey, getBy, get404, runDB, selectFirst )

import Foundation           ( Handler )
import Model                -- Accessgroup*, Note*, User*, UserAccessgroup*


data AccessMode = Read | Update | Delete


class UserLocator userLocator where
    getUserId :: userLocator -> Handler UserId

instance UserLocator UserId where
    getUserId = return

data CurrentUser = CurrentUser

instance UserLocator CurrentUser where
    getUserId _ = requireAuthId'


class NoteLocator noteLocator where
    getNote :: noteLocator -> Handler Note

instance NoteLocator Note where
    getNote = return

instance NoteLocator NoteId where
    getNote = runDB . get404


-- | Returns `()` if user has access to the note.
--   Raises `notFound` if no such noteId exists in the database.
--   Sends `CurrentUser` to login page and back if he isn't authenticated,
--     or raises `notAuthenticated` if it's an API client.
--   Raises `permissionDenied` if note is valid, user is valid,
--     but he doesn't have such permission.
authorize ::
    (UserLocator userLocator, NoteLocator noteLocator) =>
    AccessMode -> userLocator -> noteLocator -> Handler ()
authorize _ userLoc noteLoc = do
    -- access mode really doesn't matter yet
    userId <- getUserId userLoc
    note <- getNote noteLoc
    when (userId /= noteAuthor note) $
        permissionDenied "User is not the author of this note"


-- | Returns `()` if user is in `admin` access group.
--   Sends user to login page and back if he isn't authenticated,
--     or raises `notAuthenticated` if it's an API client.
--   Raises `permissionDenied` if user is valid,
--     but doesn't belong to the admin group.
authorizeAdmin :: Handler ()
authorizeAdmin = do
    userId <- requireAuthId'
    userIsAdmin <- runDB $ do
        -- TODO create `admin` group at startup, instead of error
        -- TODO get `admin` group id at startup
        adminGroupId <- getBy (UniqueAccessgroup "admin")
                        <&> ( fromMaybe (error "no group `admin` in db")
                              >>> entityKey )
        exists  [ UserAccessgroupUser ==. userId
                , UserAccessgroupGroup ==. adminGroupId
                ]
    when (not userIsAdmin) $
        permissionDenied "You have no admin priviledges"
  where
    exists filters = isJust <$> selectFirst filters []
