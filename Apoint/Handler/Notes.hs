{-# LANGUAGE  DeriveGeneric
            , NamedFieldPuns
  #-}

module Handler.Notes where

import Data.Aeson as Json
import Data.Text as Text
import GHC.Generics (Generic)

import Local.Data.Text as Text

import Import


data CreateNoteRequest = CreateNoteRequest
    { author :: Text -- User.ident
    , content :: Text
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
    request <- do
        parsedRequest <- parseJsonBody
        case parsedRequest of
            Json.Error errorDescription ->
                invalidArgs [Text.pack errorDescription]
            Json.Success request ->
                return request

    -- TODO authorId <- getCurrentUser
    let authorIdent = author request
        noteContent = content request

    authorId <- do
        mAuthor <- runDB $ getBy $ UniqueUser authorIdent
        case mAuthor of
            Nothing -> do
                let errMsg =
                        mconcat ["user ", Text.show authorIdent, " not found"]
                invalidArgs [errMsg]
            Just (Entity authorId _) ->
                return authorId

    createdNoteId <- runDB $ insert $ Note noteContent authorId
    returnJson CreateNoteResponse{noteId = createdNoteId}
