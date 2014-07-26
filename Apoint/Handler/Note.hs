module Handler.Note where

import Local.Yesod.Auth (requireAuthId')

import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    _ <- requireAuthId'
    -- TODO check acl
    note <- runDB $ get404 noteId
    let contentHtml = noteContentHtml note
    defaultLayout $(widgetFile "note")
