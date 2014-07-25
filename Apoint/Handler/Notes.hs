{-# LANGUAGE  ConstraintKinds
            , DeriveGeneric
            , NamedFieldPuns
  #-}

module Handler.Notes where

import Data.Aeson             as Json
import Data.Text              as Text (pack)
import GHC.Generics                   (Generic)
import Local.Yesod.Auth               (requireAuthId')

import Import


getNotesR :: Handler Html
getNotesR = do
    userId <- requireAuthId'
    notes <- runDB $
        selectList
            [NoteAuthor ==. userId]
            [LimitTo $ notesOnAPage + 1] -- one for pagination
    defaultLayout $ $(widgetFile "notes")

    where
        notesOnAPage = 20


data CreateNoteRequest = CreateNoteRequest
    { content :: Text
    }
        deriving (Generic)

instance FromJSON CreateNoteRequest


data CreateNoteResponse = CreateNoteResponse
    { noteId :: NoteId
    }
        deriving (Generic)

instance ToJSON CreateNoteResponse


postNotesR :: Handler Value
postNotesR = do
    userId <- requireAuthId'

    CreateNoteRequest{content} <- do
        parsedRequest <- parseJsonBody
        case parsedRequest of
            Json.Error errorDescription ->
                invalidArgs [Text.pack errorDescription]
            Json.Success request ->
                return request

    noteId <- runDB $ insert $ Note{noteContent = content, noteAuthor = userId}
    returnJson $ CreateNoteResponse noteId
        -- TODO return 201 CREATE + url to created note
