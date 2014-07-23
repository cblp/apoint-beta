{-# LANGUAGE  DeriveGeneric
            , NamedFieldPuns
  #-}

module Handler.Notes where

import Import

import Data.Aeson as Json
import Data.Text as Text
import GHC.Generics (Generic)


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
    parsedRequest <- parseJsonBody
    case parsedRequest of
        Json.Error errorDescription ->
            invalidArgs [ "request body must be an object"
                        , Text.pack errorDescription ]
        Json.Success request -> do
            -- TODO authorId <- getCurrentUser
            let authorIdent = author request
                noteContent = content request
            mAuthor <- runDB $ getBy $ UniqueUser authorIdent
            case mAuthor of
                Nothing -> invalidArgs ["user not found", authorIdent]
                Just (Entity authorId _) -> do
                    createdNoteId <- runDB $ insert $ Note noteContent authorId
                    returnJson CreateNoteResponse{noteId = createdNoteId}
