module Widgets.Note where

import Handler.Link
import Model.Note

import Import


data NoteslistMode  = SelectedNotes
                    | NotesLinkedTo NoteId
                    | NotesLinkedFrom NoteId


emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()


editableNoteWidget :: Entity Note -> Handler Widget
editableNoteWidget (Entity noteId note) = do
    (ndfWidget, ndfEnctype) <- generateFormPost emptyForm
    return $(widgetFile "noteview")


notesListWidget :: NoteslistMode -> Html -> [Entity Note] -> Handler Widget
notesListWidget mode title notes = do
    (linkWidget, enctype) <- generateFormPost noteLinkForm
    let mLinkRoute = case mode of
            NotesLinkedFrom noteId  -> Just $ LinkFromCreateR noteId
            NotesLinkedTo   noteId  -> Just $ LinkToCreateR   noteId
            _                       -> Nothing
    return $(widgetFile "noteslist")
