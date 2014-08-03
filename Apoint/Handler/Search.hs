module Handler.Search where

import Data.Aeson.TH
import Data.Maybe (fromMaybe)

import Local.Yesod.Auth (requireAuthId')

import Model.Note
import Widgets.Note

import Import


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
        selectList  [NoteAuthor ==. userId, NoteContent `contains_i` query]
                    [LimitTo limit]
    return (query, notes)


getSearchR :: Handler TypedContent
getSearchR = do
    notesOnAPage <- extraNotesOnAPage <$> getExtra
    (query, notes) <- getSearchI "query" (notesOnAPage + 1) -- one for pagination
    selectRep $ do
        provideRep $
            defaultLayout' query =<<
                notesListWidget SelectedNotes
                    [shamlet|Search results for <em>#{query}</em>|]
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
