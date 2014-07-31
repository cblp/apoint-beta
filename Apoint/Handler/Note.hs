module Handler.Note where

import Access
import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    note <- runDB $ get404 noteId
    authorize Read CurrentUser note

    let contentHtml = noteContentHtml note
    defaultLayout $(widgetFile "note")


postNoteDeleteR :: NoteId -> Handler ()
postNoteDeleteR noteId = do
    note <- runDB $ get404 noteId
    authorize Delete CurrentUser note

    runDB $ delete noteId

    setMessage $ "Deleted \"" <> (toHtml $ noteContentShort note) <> "\""
    redirect $ NotesR
