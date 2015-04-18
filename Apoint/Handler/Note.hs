module Handler.Note where

import Data.Monoid          ( (<>) )
import Prelude.Extended
import Text.Blaze.Html      ( Html, toHtml )
import Yesod.Auth.Extended  ( requireAuthId' )
import Yesod.Core           ( defaultLayout, redirect, setMessage )
import Yesod.Form           ( Textarea (..) )
import Yesod.Persist        ( (=.), (==.)
                            , Entity (..), SelectOpt (LimitTo)
                            , delete, deleteWhere
                            , get404
                            , insert, insert_
                            , runDB
                            , selectList
                            , update
                            )

import Access               ( AccessMode (Delete, Read, Update)
                            , CurrentUser (CurrentUser)
                            , authorize
                            )
import Form                 ( emptyForm, runFormPostChecked )
import Form.Note            ( noteContentForm )
import Foundation
import Model
import Model.Note           ( noteContentShort, noteSiblings )
import Settings             ( extraNotesOnAPage )
import Widget.Note          ( NoteslistMode ( NotesLinkedFrom
                                            , NotesLinkedFromNew
                                            , NotesLinkedTo
                                            , NotesLinkedToNew
                                            , SelectedNotes
                                            )
                            , UserIntent (UserIntentExisting, UserIntentNew)
                            , UserIntentExisting (Edit, View)
                            , UserIntentNew (CreateFree, CreateRel)
                            , makeNewNoteWidget
                            , makeNoteContentViewWidget
                            , makeNoteContentEditWidget
                            , makeNotesListWidget
                            , workareaWidget
                            )


getNoteR :: NoteId -> Handler Html
getNoteR noteId = notePage $ UserIntentExisting $ View noteId


postNoteArchiveR :: NoteId -> Handler ()
postNoteArchiveR noteId = do
    _ <- runFormPostChecked emptyForm

    note <- runDB $ get404 noteId
    authorize Access.Update CurrentUser note

    runDB $ update noteId [NoteArchived =. True]

    setMessage $ "Archived \"" <> toHtml (noteContentShort note) <> "\""
    redirect NotesR


postNoteDeleteR :: NoteId -> Handler ()
postNoteDeleteR noteId = do
    _ <- runFormPostChecked emptyForm

    note <- runDB $ get404 noteId
    authorize Delete CurrentUser note

    runDB $ do
        deleteWhere [NotelinkFrom ==. noteId]
        deleteWhere [NotelinkTo ==. noteId]
        delete noteId

    setMessage $ "Deleted \"" <> toHtml (noteContentShort note) <> "\""
    redirect NotesR


getNoteNewR :: Handler Html
getNoteNewR = do
    _ <- requireAuthId'
    notePage $ UserIntentNew CreateFree


getNoteNewRelR :: Rel -> NoteId -> Handler Html
getNoteNewRelR rel noteId = notePage $ UserIntentNew $ CreateRel rel noteId


postNoteNewRelR :: Rel -> NoteId -> Handler ()
postNoteNewRelR rel noteId = postNotesR' $ Just (rel, noteId)


getNotesR :: Handler Html
getNotesR = do
    notesOnAPage <- extraNotesOnAPage <$> getExtra
    userId <- requireAuthId'
    notes <- runDB $
        selectList
            [NoteAuthor ==. userId, NoteArchived ==. False]
            [LimitTo $ notesOnAPage + 1] -- one for pagination
    defaultLayout =<< makeNotesListWidget SelectedNotes notes


postNotesR :: Handler ()
postNotesR = postNotesR' Nothing


postNotesR' :: Maybe (Rel, NoteId) -> Handler ()
postNotesR' mRelNoteId = do
    userId <- requireAuthId'

    Textarea content <- runFormPostChecked $ noteContentForm Nothing

    noteId <- runDB $ do
        noteId <- insert Note
            { noteContent = content
            , noteAuthor = userId
            , noteArchived = False
            }
        case mRelNoteId of
            Nothing                 -> return ()
            Just (RelFrom,  fromId) -> insert_ $ Notelink fromId noteId
            Just (RelTo,    toId)   -> insert_ $ Notelink noteId toId
        return noteId

    redirect $ NoteR noteId


notePage :: UserIntent -> Handler Html
notePage userIntent' =
    case userIntent' of
        UserIntentExisting  userIntent -> notePageExisting  userIntent
        UserIntentNew       userIntent -> notePageNew       userIntent

    where
        notePageExisting :: UserIntentExisting -> Handler Html
        notePageExisting userIntent = do
            let (accessMode, noteId) = case userIntent of
                    View nid -> (Access.Read,   nid)
                    Edit nid -> (Access.Update, nid)
            note <- runDB $ get404 noteId
            authorize accessMode CurrentUser note

            let makeCurrentNoteWidget = case userIntent of
                    View _      -> makeNoteContentViewWidget
                    Edit _      -> makeNoteContentEditWidget

            notesBeforeCurrent <- noteSiblings [NotelinkTo   ==. noteId] notelinkFrom
            notesAfterCurrent  <- noteSiblings [NotelinkFrom ==. noteId] notelinkTo

            defaultLayout =<< curry3 workareaWidget
                <$> makeNotesListWidget (NotesLinkedTo   noteId)
                                        notesBeforeCurrent
                <*> makeCurrentNoteWidget (Entity noteId note)
                <*> makeNotesListWidget (NotesLinkedFrom noteId)
                                        notesAfterCurrent

        notePageNew :: UserIntentNew -> Handler Html
        notePageNew userIntent = do
            let mNoteId = case userIntent of
                    CreateFree            -> Nothing
                    CreateRel _ noteId    -> Just noteId
                saveR = case userIntent of
                    CreateFree            -> NotesR
                    CreateRel rel noteId  -> NoteNewRelR rel noteId
            (notesBeforeCurrent, notesAfterCurrent) <- case userIntent of
                    CreateFree            ->    return ([], [])
                    CreateRel rel noteId  -> do note <- getEntity noteId
                                                return $ case rel of
                                                    RelFrom -> ([note], [])
                                                    RelTo   -> ([], [note])

            defaultLayout =<< curry3 workareaWidget
                <$> makeNotesListWidget NotesLinkedToNew    notesBeforeCurrent
                <*> makeNewNoteWidget saveR mNoteId
                <*> makeNotesListWidget NotesLinkedFromNew  notesAfterCurrent

        getEntity :: NoteId -> Handler (Entity Note)
        getEntity noteId = do
            note <- runDB $ get404 noteId
            return $ Entity noteId note


getNoteEditR :: NoteId -> Handler Html
getNoteEditR noteId = notePage $ UserIntentExisting $ Edit noteId


postNoteR :: NoteId -> Handler ()
postNoteR noteId = do
    note <- runDB $ get404 noteId
    authorize Access.Update CurrentUser note
    Textarea content  <- runFormPostChecked $ noteContentForm Nothing
    runDB $ update noteId [NoteContent =. content]
    redirect $ NoteR noteId
