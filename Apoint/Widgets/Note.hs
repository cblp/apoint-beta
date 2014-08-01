module Widgets.Note where

import Import


data NoteslistMode = SelectedNotes | LinkedNotes


emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()


editableNoteWidget :: Entity Note -> Handler Widget
editableNoteWidget (Entity noteId note) = do
    (ndfWidget, ndfEnctype) <- generateFormPost emptyForm
    return $(widgetFile "noteview")


notesList :: [Entity Note] -> Text -> Widget
notesList notes title = do
    let mode = LinkedNotes
    $(widgetFile "noteslist")
