module Handler.Search where

import Data.Aeson.TH        ( defaultOptions, deriveToJSON, fieldLabelModifier )
import Data.Functor         ( (<$>) )
import Data.Maybe           ( fromMaybe )
import Data.Monoid          ( (<>) )
import Data.Text            ( Text )
import Yesod.Auth.Extended  ( requireAuthId' )

import Import
import Model.Note           ( noteContentShort )
import Widget.Note          ( NoteslistMode (FoundNotes), makeNotesListWidget )


data NoteSearchResult = NoteSearchResult
    { nsr_id            :: Text
    , nsr_content_short :: Text
    }
deriveToJSON defaultOptions{fieldLabelModifier = drop 4} ''NoteSearchResult

data NoteSearchSuggest = NoteSearchSuggest
    { nss_value :: Text
    , nss_label :: Text
    }
deriveToJSON
    defaultOptions{fieldLabelModifier = drop 4}
    ''NoteSearchSuggest


getSearchI :: Text -> Int -> Handler (Text, [Entity Note])
getSearchI paramName limit = do
    userId <- requireAuthId'
    query <- fromMaybe "" <$> lookupGetParam paramName
    notes <- runDB $
        selectList  [ NoteAuthor ==. userId
                    , NoteArchived ==. False
                    , NoteContent `contains_i` query
                    ]
                    [LimitTo limit]
    return (query, notes)


getSearchR :: Handler TypedContent
getSearchR = do
    notesOnAPage <- extraNotesOnAPage <$> getExtra
    (query, notes) <- getSearchI "query" (notesOnAPage + 1) -- one for pagination
    selectRep $ do
        provideRep $
            defaultLayout' query =<< makeNotesListWidget  (FoundNotes query)
                                                          notes
        provideRep $
            returnJson  [ NoteSearchResult
                            { nsr_id            = toPathPiece noteId
                            , nsr_content_short = noteContentShort note
                            }
                        | Entity noteId note <- notes ]


getSearchSuggestR :: Handler Value
getSearchSuggestR = do
    noteSuggestSize <- extraNoteSuggestSize <$> getExtra
    (_, notes) <- getSearchI "term" noteSuggestSize
    returnJson  [ noteContentShort note <> " [" <> toPathPiece noteId <> "]"
                | Entity noteId note <- notes ]
