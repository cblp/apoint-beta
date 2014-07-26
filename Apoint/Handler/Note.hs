module Handler.Note where

import Control.Monad (when)

import Local.Yesod.Auth (requireAuthId')

import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    userId <- requireAuthId'
    note <- runDB $ get404 noteId

    when (userId /= noteAuthor note) $
        permissionDenied "You are not the author of this note"

    let contentHtml = noteContentHtml note
    defaultLayout $(widgetFile "note")
