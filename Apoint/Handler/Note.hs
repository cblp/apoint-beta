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
    authorize Delete CurrentUser noteId

    runDB $ delete noteId

    redirect $ NotesR
