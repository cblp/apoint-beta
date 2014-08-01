module Handler.Note where

import Control.Monad (forM)

import Access
import Import


notesList :: [Entity Note] -> Widget
notesList notes = $(widgetFile "notes")


data NoteDelete = NoteDelete

noteDeleteForm :: Html -> MForm Handler (FormResult NoteDelete, Widget)
noteDeleteForm = renderDivs $ pure NoteDelete


noteSiblings ::
    [Filter Notelink] -> (Notelink -> NoteId) -> Handler [Entity Note]
noteSiblings filters fieldForSelect =
    -- TODO remove runDB from here, replacing Handler with DB monad
    runDB $ do
        links <- selectList filters []
        forM links $ \link -> do
            let nid = fieldForSelect $ entityVal link
            Just n <- get nid
            return $ Entity nid n


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    note <- runDB $ get404 noteId
    authorize Read CurrentUser note

    notesBeforeCurrent <- noteSiblings [NotelinkTo   ==. noteId] notelinkFrom
    notesAfterCurrent  <- noteSiblings [NotelinkFrom ==. noteId] notelinkTo

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

    runDB $ do
        deleteWhere [NotelinkFrom ==. noteId]
        deleteWhere [NotelinkTo ==. noteId]
        delete noteId

    setMessage $ "Deleted \"" <> (toHtml $ noteContentShort note) <> "\""
    redirect $ NotesR
