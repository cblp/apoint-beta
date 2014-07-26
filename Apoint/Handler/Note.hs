module Handler.Note where

import Local.Yesod.Auth (requireAuthId')

import AccessControl
import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    userId <- requireAuthId'
    note <- runDB $ get404 noteId
    checkUserCanRead userId note

    let contentHtml = noteContentHtml note
    defaultLayout $(widgetFile "note")


postNoteDeleteR :: NoteId -> Handler ()
postNoteDeleteR noteId = do
    userId <- requireAuthId'
    note <- runDB $ get404 noteId
    checkUserCanWrite userId note

    runDB $ delete noteId

    redirect $ NotesR
