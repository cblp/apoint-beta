module Widgets.Note where

import Yesod.Form.Jquery

import Model.Note

import Import


data NoteslistMode = SelectedNotes | LinkedNotes


emptyForm :: Html -> MForm Handler (FormResult (), Widget)
emptyForm = renderDivs $ pure ()


noteLinkForm :: Html -> MForm Handler (FormResult Text, Widget)
noteLinkForm = renderDivsNoLabels $
    areq (jqueryAutocompleteField SearchSuggestR) "" Nothing


editableNoteWidget :: Entity Note -> Handler Widget
editableNoteWidget (Entity noteId note) = do
    (ndfWidget, ndfEnctype) <- generateFormPost emptyForm
    return $(widgetFile "noteview")


notesListWidget :: NoteslistMode -> Html -> [Entity Note] -> Handler Widget
notesListWidget mode title notes = do
    (linkWidget, _) <- generateFormGet' noteLinkForm
    return $(widgetFile "noteslist")
