module Widget.Note where

import Data.Aeson         ( Value, toJSON )
import Data.Monoid        ( (<>) )
import Data.Maybe         ( isJust )
import Data.Text          ( Text )
import Prelude
import Text.Hamlet        ( shamlet )
import Yesod.Core         ( newIdent )
import Yesod.Form         ( generateFormPost )
import Yesod.Persist      ( Entity (..) )
import Yesod.Routes.Class ( Route )

import Form.Note          ( noteContentForm )
import Foundation
import Handler.Link       ( noteLinkForm )
import Model              ( Note (..), NoteId )
import Model.Note         ( noteContentHtml, noteContentShort )
import Settings           ( widgetFile )
import Widget             ( makePostButton )


data NoteslistMode  = SelectedNotes
                    | NotesLinkedTo NoteId
                    | NotesLinkedFrom NoteId
                    | NotesLinkedToNew
                    | NotesLinkedFromNew
                    | FoundNotes Text -- ^ search query


makeNotesListWidget :: NoteslistMode -> [Entity Note] -> Handler Widget
makeNotesListWidget mode notes = do
    (linkWidget, enctype) <- generateFormPost noteLinkForm
    let mRoutes = case mode of
            NotesLinkedFrom noteId  ->
                Just (LinkCreateR RelFrom noteId, NoteNewRelR RelFrom noteId)
            NotesLinkedTo   noteId  ->
                Just (LinkCreateR RelTo   noteId, NoteNewRelR RelTo   noteId)
            _                       ->
                Nothing
        title = case mode of
            SelectedNotes       -> "Notes"
            NotesLinkedTo _     -> "Before →"
            NotesLinkedFrom _   -> "→ After"
            FoundNotes query    -> [shamlet|Search results for <em>#{query}<em>|]
            NotesLinkedToNew    ->
                if isJust mRoutes || not (null notes) then "Before →"
                                                      else ""
            NotesLinkedFromNew  ->
                if isJust mRoutes || not (null notes) then "→ After"
                                                      else ""
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
data UserIntentNew      = CreateFree | CreateRel Rel NoteId
data UserIntent         = UserIntentExisting  UserIntentExisting
                        | UserIntentNew       UserIntentNew


makeNoteContentViewWidget :: Entity Note -> Handler Widget
makeNoteContentViewWidget (Entity noteId note) = do
    archiveBtn <- makePostButton (NoteArchiveR noteId) "Archive"
    deleteBtn <- makePostButton (NoteDeleteR noteId)
        [shamlet|<i.icon-remove></i>Delete permanently|]
    return $(widgetFile "noteview")


makeNoteContentEditWidget :: Entity Note -> Handler Widget
makeNoteContentEditWidget (Entity noteId note) = do
    let content = noteContent note
        saveR = NoteR noteId
        cancelR = NoteR noteId
    (formWidget, enctype) <- generateFormPost $ noteContentForm (Just content)
    return $(widgetFile "noteedit")


makeNewNoteWidget :: Route App -> Maybe NoteId -> Handler Widget
makeNewNoteWidget saveR mReturnNote = do
    let cancelR = maybe NotesR NoteR mReturnNote
    (formWidget, enctype) <- generateFormPost $ noteContentForm Nothing
    return $(widgetFile "noteedit")
