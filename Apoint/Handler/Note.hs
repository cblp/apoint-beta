module Handler.Note where

import Local.Yesod.Auth (requireAuthId')

import Access
import Form
import Form.Note
import Model.Note
import Widget.Note

import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = notePage $ UserIntentExisting $ View noteId


postNoteArchiveR :: NoteId -> Handler ()
postNoteArchiveR noteId = do
    ((formResult, _), _) <- runFormPost emptyForm
    case formResult of
        FormSuccess () -> return ()
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure errors -> invalidArgs errors

    note <- runDB $ get404 noteId
    authorize Access.Update CurrentUser note

    runDB $ update noteId [NoteArchived =. True]

    setMessage $ "Archived \"" <> toHtml (noteContentShort note) <> "\""
    redirect NotesR


postNoteDeleteR :: NoteId -> Handler ()
postNoteDeleteR noteId = do
    ((formResult, _), _) <- runFormPost emptyForm
    case formResult of
        FormSuccess () -> return ()
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure errors -> invalidArgs errors

    note <- runDB $ get404 noteId
    authorize Delete CurrentUser note

    runDB $ do
        deleteWhere [NotelinkFrom ==. noteId]
        deleteWhere [NotelinkTo ==. noteId]
        delete noteId

    setMessage $ "Deleted \"" <> toHtml (noteContentShort note) <> "\""
    redirect NotesR


getNoteNewR :: Handler Html
getNoteNewR = notePage $ UserIntentNew CreateFree


getNoteNewFromR :: NoteId -> Handler Html
getNoteNewFromR = notePage . UserIntentNew . CreateAfter


postNoteNewFromR :: NoteId -> Handler ()
postNoteNewFromR noteId = postNotesR' Nothing (Just noteId)


getNoteNewToR :: NoteId -> Handler Html
getNoteNewToR = notePage . UserIntentNew . CreateBefore


postNoteNewToR :: NoteId -> Handler ()
postNoteNewToR noteId = postNotesR' (Just noteId) Nothing


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
postNotesR = postNotesR' Nothing Nothing


postNotesR' :: Maybe NoteId -> Maybe NoteId -> Handler ()
postNotesR' mNoteLinkTo mNoteLinkFrom = do
    userId <- requireAuthId'

    ((formResult, _), _) <- runFormPost $ noteContentForm Nothing
    content <- case formResult of
        FormSuccess (Textarea content) -> return content
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure errors -> invalidArgs errors

    noteId <- runDB $ do
        noteId <- insert Note
            { noteContent = content
            , noteAuthor = userId
            , noteArchived = False
            }
        case mNoteLinkTo of
            Nothing -> return ()
            Just noteTo ->
                insert_ $ Notelink noteId noteTo
        case mNoteLinkFrom of
            Nothing -> return ()
            Just noteFrom ->
                insert_ $ Notelink noteFrom noteId
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
                    CreateFree          -> Nothing
                    CreateBefore noteId -> Just noteId
                    CreateAfter  noteId -> Just noteId
                saveR = case userIntent of
                    CreateFree          -> NotesR
                    CreateBefore noteId -> NoteNewToR   noteId
                    CreateAfter  noteId -> NoteNewFromR noteId
            (notesBeforeCurrent, notesAfterCurrent) <- case userIntent of
                    CreateFree          ->    return ([], [])
                    CreateBefore noteId -> do e <- getEntity noteId
                                              return ([], [e])
                    CreateAfter  noteId -> do e <- getEntity noteId
                                              return ([e], [])

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

    ((formResult, _), _) <- runFormPost $ noteContentForm Nothing
    Textarea content <-
        case formResult of
            FormSuccess textarea ->
                return textarea
            FormMissing ->
                invalidArgs ["FormMissing"]
            FormFailure errors ->
                invalidArgs errors

    runDB $ update noteId [NoteContent =. content]

    redirect $ NoteR noteId
