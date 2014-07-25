module Handler.Note where

import Local.Yesod.Auth (requireAuthId')

import Import


getNoteR :: NoteId -> Handler Value
getNoteR noteId = do
    _ <- requireAuthId'
    note <- runDB $ get404 noteId
    returnJson $ noteContent note
