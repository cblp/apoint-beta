module Handler.Note where

import Local.Yesod.Auth (requireAuthId')

import Access
import Model.Note
import Widgets.Note

import Import


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    note <- runDB $ get404 noteId
    let noteEntity = Entity noteId note
    authorize Read CurrentUser note

    notesBeforeCurrent <- noteSiblings [NotelinkTo   ==. noteId] notelinkFrom
    notesAfterCurrent  <- noteSiblings [NotelinkFrom ==. noteId] notelinkTo

    leftColumnWidget  <- notesListWidget  (NotesLinkedTo    noteId)
                                          "Before"
                                          notesBeforeCurrent
    rightColumnWidget <- notesListWidget  (NotesLinkedFrom  noteId)
                                          "After"
                                          notesAfterCurrent
    centerColumnWidget <- editableNoteWidget noteEntity
    defaultLayout $(widgetFile "notesview")


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


getNotesR :: Handler Html
getNotesR = do
    notesOnAPage <- extraNotesOnAPage <$> getExtra
    userId <- requireAuthId'
    notes <- runDB $
        selectList
            [NoteAuthor ==. userId]
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

    noteId <- runDB $ insert Note{noteContent = content, noteAuthor = userId}

    redirect $ NoteR noteId
