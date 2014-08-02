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


getSearchR :: Handler TypedContent
getSearchR = do
    notesOnAPage <- extraNotesOnAPage <$> getExtra
    userId <- requireAuthId'
    query <- fromMaybe "" <$> lookupGetParam "query"
    notes <- runDB $
        selectList  [NoteAuthor ==. userId, NoteContent `contains_i` query]
                    [LimitTo $ notesOnAPage + 1] -- one for pagination

    selectRep $ do
        provideRep $ do
            let title = [shamlet|Search results for <em>#{query}</em>|]
                mode = SelectedNotes
            defaultLayout' query $(widgetFile "noteslist") -- TODO make a function
        provideRep $
            returnJson  [ NoteSearchResult
                            { nsr_id            = toPathPiece noteId
                            , nsr_content_short = noteContentShort note
                            }
                        | Entity noteId note <- notes ]
