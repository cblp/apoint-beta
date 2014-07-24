{-# LANGUAGE  DeriveGeneric
            , NamedFieldPuns
  #-}

module Handler.Notes where

import Data.Aeson             as Json
import Data.Aeson.Encode      as Json
import Data.Text              as Text (pack)
import Data.Text.Lazy         as Text (toStrict)
import Data.Text.Lazy.Builder as Text (toLazyText)
import GHC.Generics                   (Generic)

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
    CreateNoteRequest{author, content} <- do
        parsedRequest <- parseJsonBody
        case parsedRequest of
            Json.Error errorDescription ->
                invalidArgs [Text.pack errorDescription]
            Json.Success request ->
                return request

    -- TODO authorize; author <- authUser

    authorId <- do
        mAuthor <- runDB $ getBy $ UniqueUser author
        case mAuthor of
            Nothing -> do
                invalidArgs ["user " ⊕ toJsonText author ⊕ " not found"]
            Just (Entity authorId _) ->
                return authorId

    noteId <- runDB $ insert $ Note content authorId
    returnJson $ CreateNoteResponse noteId
        -- TODO return 201 CREATE + url to created note

  where

    toJsonText :: ToJSON a => a -> Text
    toJsonText =
        Json.toJSON
        >>> Json.encodeToTextBuilder
        >>> Text.toLazyText
        >>> Text.toStrict
