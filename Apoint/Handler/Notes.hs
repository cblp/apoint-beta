{-# LANGUAGE  ConstraintKinds
            , DeriveGeneric
            , NamedFieldPuns
  #-}

module Handler.Notes where

import Data.Aeson             as Json
import Data.Text              as Text (pack)
import GHC.Generics                   (Generic)
import Yesod.Auth

import Import


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


-- | Similar to 'requireAuthId',
-- but returns "HTTP 401 Unauthorized" if user is not authorized.
requireAuthId' ::
    YesodAuthPersist master => HandlerT master IO (AuthId master)
requireAuthId' =
    maybeAuthId >>= maybe handleAuthLack return
    where
        handleAuthLack = do
            aj <- acceptsJson
            if aj then notAuthenticated else redirectLogin

        redirectLogin :: Yesod master => HandlerT master IO a
        redirectLogin = do
            y <- getYesod
            setUltDestCurrent
            case authRoute y of
                Just z -> redirect z
                Nothing -> permissionDenied "Please configure authRoute"
