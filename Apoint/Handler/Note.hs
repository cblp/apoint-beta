module Handler.Note where

import Local.Yesod.Auth (requireAuthId')

import Access
import Form
import Form.Note
import Model.Note
import Widget.Note

import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = notePage ViewMode noteId


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


data NoteNewInput = NoteNewInput
    { nnContent :: Textarea
    }

noteNewForm :: Html -> MForm Handler (FormResult NoteNewInput, Widget)
noteNewForm = renderDivs $ NoteNewInput
    <$> areq textareaField "Content" Nothing

noteNewPage :: Widget -> Enctype -> Handler Html
noteNewPage widget enctype =
    defaultLayout
        [whamlet|
            <h1>New note
            <form method=post action=@{NotesR} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]


getNoteNewR :: Handler Html
getNoteNewR = do
    _ <- requireAuthId'
    (widget, enctype) <- generateFormPost noteNewForm
    noteNewPage widget enctype


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

    ((formResult, _), _) <- runFormPost noteNewForm
    content <- case formResult of
        FormSuccess NoteNewInput{nnContent} -> return $ unTextarea nnContent
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure errors -> invalidArgs errors

    noteId <- runDB $ insert Note
        { noteContent = content
        , noteAuthor = userId
        , noteArchived = False
        }

    redirect $ NoteR noteId


notePage :: UiMode -> NoteId -> Handler Html
notePage uiMode noteId = do
    note <- runDB $ get404 noteId
    let accessMode = case uiMode of
            ViewMode -> Read
            EditMode -> Access.Update
    authorize accessMode CurrentUser note

    let makeCurrentNoteWidget = case uiMode of
            ViewMode -> makeNoteContentViewWidget
            EditMode -> makeNoteContentEditWidget

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
getNoteEditR noteId = notePage EditMode noteId


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
