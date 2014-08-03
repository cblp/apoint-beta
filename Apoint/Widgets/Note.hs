module Widgets.Note where

import Model.Note

import Import


data NoteslistMode = SelectedNotes | LinkedNotes


emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()


editableNoteWidget :: Entity Note -> Handler Widget
editableNoteWidget (Entity noteId note) = do
    (ndfWidget, ndfEnctype) <- generateFormPost emptyForm
    return $(widgetFile "noteview")


notesListWidget :: NoteslistMode -> Html -> [Entity Note] -> Widget
notesListWidget mode title notes = $(widgetFile "noteslist")
