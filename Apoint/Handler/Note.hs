module Handler.Note where

import Access
import Import


data NoteDelete = NoteDelete

noteDeleteForm :: Html -> MForm Handler (FormResult NoteDelete, Widget)
noteDeleteForm = renderDivs $ pure NoteDelete


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    note <- runDB $ get404 noteId
    authorize Read CurrentUser note

    let contentHtml = noteContentHtml note
    (noteDeleteWidget, noteDeleteEnctype) <- generateFormPost noteDeleteForm
    defaultLayout $(widgetFile "note")


postNoteDeleteR :: NoteId -> Handler ()
postNoteDeleteR noteId = do
    ((formResult, _), _) <- runFormPost noteDeleteForm
    case formResult of
        FormSuccess NoteDelete -> return ()
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure errors -> invalidArgs errors

    note <- runDB $ get404 noteId
    authorize Delete CurrentUser note

    runDB $ delete noteId

    setMessage $ "Deleted \"" <> (toHtml $ noteContentShort note) <> "\""
    redirect $ NotesR
