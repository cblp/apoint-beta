module Widget.Note where

import Form
import Form.Note
import Handler.Link
import Model.Note

import Import


data NoteslistMode  = SelectedNotes
                    | NotesLinkedTo NoteId
                    | NotesLinkedFrom NoteId


makeNotesListWidget :: NoteslistMode -> Html -> [Entity Note] -> Handler Widget
makeNotesListWidget mode title notes = do
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


data UserIntentExisting = View NoteId | Edit NoteId
data UserIntentNew      = CreateFree
                        -- | CreateAfter NoteId
                        -- | CreateBefore NoteId
data UserIntent         = UserIntentExisting  UserIntentExisting
                        | UserIntentNew       UserIntentNew


makeNoteContentViewWidget :: Entity Note -> Handler Widget
makeNoteContentViewWidget (Entity noteId note) = do
    (ndfWidget, ndfEnctype) <- generateFormPost emptyForm
    return $(widgetFile "noteview")


makeNoteContentEditWidget :: Entity Note -> Handler Widget
makeNoteContentEditWidget (Entity noteId note) = do
    let content = noteContent note
        saveR = NoteR noteId
        cancelR = NoteR noteId
    (formWidget, enctype) <- generateFormPost $ noteContentForm (Just content)
    return $(widgetFile "noteedit")


makeNewNoteWidget :: Handler Widget
makeNewNoteWidget = do
    let saveR = NotesR
        cancelR = NotesR
    (formWidget, enctype) <- generateFormPost $ noteContentForm Nothing
    return $(widgetFile "noteedit")
