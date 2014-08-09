module Widget.Note where

import Form
import Form.Note
import Handler.Link
import Model.Note

import Import


data NoteslistMode  = SelectedNotes
                    | NotesLinkedTo NoteId
                    | NotesLinkedFrom NoteId


notesListWidget :: NoteslistMode -> Html -> [Entity Note] -> Handler Widget
notesListWidget mode title notes = do
    (linkWidget, enctype) <- generateFormPost noteLinkForm
    let mRoutes = case mode of
            NotesLinkedFrom noteId  ->
                Just (LinkFromCreateR noteId, NoteNewFromR noteId)
            NotesLinkedTo   noteId  ->
                Just (LinkToCreateR   noteId, NoteNewToR   noteId)
            _                       ->
                Nothing
    linkWidgetShowerId <- newIdent
    linkWidgetFormId <- newIdent
    return $(widgetFile "noteslist")


workareaWidget :: (Widget, Widget, Widget) -> Widget
workareaWidget (leftWidget, centerWidget, rightWidget) =
    $(widgetFile "workarea")


jsIdSelector :: Text -> Value
jsIdSelector = toJSON . ("#" <>)


jsId :: Text -> Value
jsId = toJSON


data UserIntent = View NoteId
                | Edit NoteId
                | CreateFree
                -- | CreateAfter NoteId
                -- | CreateBefore NoteId


makeNoteContentViewWidget :: Entity Note -> Handler Widget
makeNoteContentViewWidget (Entity noteId note) = do
    (ndfWidget, ndfEnctype) <- generateFormPost emptyForm
    return $(widgetFile "noteview")


makeNoteContentEditWidget :: Entity Note -> Handler Widget
makeNoteContentEditWidget (Entity noteId note) = do
    let content = noteContent note
    (formWidget, enctype) <-
        generateFormPost $ noteContentForm (Just content)
    return $(widgetFile "noteedit")
