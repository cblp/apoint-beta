module Handler.Note where

import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    note <- runDB $ get404 noteId
    return $ toHtml $ noteContent note
