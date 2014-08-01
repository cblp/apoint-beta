module Handler.Note where

import Control.Monad (forM)

import Local.Yesod.Auth (requireAuthId')

import Access
import Import


notesList :: [Entity Note] -> Text -> Widget
notesList notes title = do
    let mode = LinkedNotes
    $(widgetFile "noteslist")


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


editableNoteWidget :: Entity Note -> Handler Widget
editableNoteWidget (Entity noteId note) = do
    (ndfWidget, ndfEnctype) <- generateFormPost noteDeleteForm
    return $(widgetFile "noteview")


getNoteR :: NoteId -> Handler Html
getNoteR noteId = do
    note <- runDB $ get404 noteId
    let noteEntity = Entity noteId note
    authorize Read CurrentUser note

    notesBeforeCurrent <- noteSiblings [NotelinkTo   ==. noteId] notelinkFrom
    notesAfterCurrent  <- noteSiblings [NotelinkFrom ==. noteId] notelinkTo

    let leftColumnWidget   = notesList notesBeforeCurrent "Before"
        rightColumnWidget  = notesList notesAfterCurrent  "After"
    centerColumnWidget <- editableNoteWidget noteEntity
    defaultLayout $(widgetFile "notesview")


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


data NoteslistMode = SelectedNotes | LinkedNotes


getNotesR :: Handler Html
getNotesR = do
    userId <- requireAuthId'
    notes <- runDB $
        selectList
            [NoteAuthor ==. userId]
            [LimitTo $ notesOnAPage + 1] -- one for pagination
    let mode = SelectedNotes
        title = "Next" :: Text
    defaultLayout $(widgetFile "noteslist")

    where
        notesOnAPage = 20


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
