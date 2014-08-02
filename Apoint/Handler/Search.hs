module Handler.Search where

import Data.Maybe (fromMaybe)

import Local.Yesod.Auth (requireAuthId')

import Model.Note
import Widgets.Note

import Import


getSearchR :: Handler Html
getSearchR = do
    notesOnAPage <- extraNotesOnAPage <$> getExtra
    userId <- requireAuthId'
    query <- fromMaybe "" <$> lookupGetParam "query"
    notes <- runDB $
        selectList
            [NoteAuthor ==. userId, NoteContent `contains_i` query]
            [LimitTo $ notesOnAPage + 1] -- one for pagination

    let title = [shamlet|Search results for <em>#{query}</em>|]
        mode = SelectedNotes
    defaultLayout' query $(widgetFile "noteslist") -- TODO make a function
