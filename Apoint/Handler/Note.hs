module Handler.Note where

import Local.Yesod.Auth (requireAuthId')

import Access
import Form
import Form.Note
import Model.Note
import Widget.Note

import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = notePage $ View noteId


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
getNoteNewR = notePage CreateFree


getNoteNewFromR :: NoteId -> Handler Html
getNoteNewFromR _ =
    -- TODO UNIMPLEMENTED
    getNoteNewR


getNoteNewToR :: NoteId -> Handler Html
getNoteNewToR _ =
    -- TODO UNIMPLEMENTED
    getNoteNewR


getNotesR :: Handler Html
getNotesR = do
    notesOnAPage <- extraNotesOnAPage <$> getExtra
    userId <- requireAuthId'
    notes <- runDB $
        selectList
            [NoteAuthor ==. userId, NoteArchived ==. False]
            [LimitTo $ notesOnAPage + 1] -- one for pagination
    defaultLayout =<< notesListWidget SelectedNotes "Next" notes


postNotesR :: Handler ()
postNotesR = do
    userId <- requireAuthId'

    ((formResult, _), _) <- runFormPost $ noteContentForm Nothing
    content <- case formResult of
        FormSuccess (Textarea content) -> return content
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure errors -> invalidArgs errors

    noteId <- runDB $ insert Note
        { noteContent = content
        , noteAuthor = userId
        , noteArchived = False
        }

    redirect $ NoteR noteId


notePage :: UserIntent -> Handler Html
notePage userIntent = do
    let (accessMode, noteId) = case userIntent of
            View nid -> (Access.Read,   nid)
            Edit nid -> (Access.Update, nid)
    note <- runDB $ get404 noteId
    authorize accessMode CurrentUser note

    let makeCurrentNoteWidget = case userIntent of
            View _      -> makeNoteContentViewWidget
            Edit _      -> makeNoteContentEditWidget
            CreateFree  -> makeNoteContentEditWidget

    notesBeforeCurrent <- noteSiblings [NotelinkTo   ==. noteId] notelinkFrom
    notesAfterCurrent  <- noteSiblings [NotelinkFrom ==. noteId] notelinkTo

    w <- curry3 workareaWidget
        <$> notesListWidget (NotesLinkedTo   noteId)
                            "Before →"
                            notesBeforeCurrent
        <*> makeCurrentNoteWidget (Entity noteId note)
        <*> notesListWidget (NotesLinkedFrom noteId)
                            "→ After"
                            notesAfterCurrent
    defaultLayout w


getNoteEditR :: NoteId -> Handler Html
getNoteEditR noteId = notePage $ Edit noteId


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
