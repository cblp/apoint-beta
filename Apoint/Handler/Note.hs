module Handler.Note where

import Import


getNoteR :: NoteId -> Handler Value
getNoteR noteId = do
    note <- runDB $ get404 noteId
    returnJson $ noteContent note
