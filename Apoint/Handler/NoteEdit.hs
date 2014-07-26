{-# LANGUAGE NamedFieldPuns
  #-}

module Handler.NoteEdit where

import Local.Yesod.Auth (requireAuthId')

import AccessControl
import Import


data NoteContentEditInput = NoteContentEditInput
    { nceContent    :: Textarea
    }

noteContentEditForm ::
    Maybe Text ->
    Html -> MForm Handler (FormResult NoteContentEditInput, Widget)
noteContentEditForm mContentOld =
    renderDivs $ NoteContentEditInput
        <$> areq textareaField  "" (Textarea <$> mContentOld)

noteContentEditPage :: NoteId -> Widget -> Enctype -> Handler Html
noteContentEditPage noteId widget enctype =
    defaultLayout
        [whamlet|
            <h1>Editing note
            <form method=post action=@{NoteR noteId} enctype=#{enctype}>
                ^{widget}
                <button>Submit
        |]


getNoteEditR :: NoteId -> Handler Html
getNoteEditR noteId = do
    userId <- requireAuthId'
    note <- runDB $ get404 noteId
    checkUserCanRead userId note

    let content = noteContent note

    (widget, enctype) <- generateFormPost $ noteContentEditForm (Just content)
    noteContentEditPage noteId widget enctype


postNoteR :: NoteId -> Handler ()
postNoteR noteId = do
    userId <- requireAuthId'
    note <- runDB $ get404 noteId
    checkUserCanWrite userId note

    ((formResult, _), _) <- runFormPost $ noteContentEditForm Nothing
    NoteContentEditInput{nceContent = Textarea content} <-
        case formResult of
            FormSuccess nce ->
                return nce
            FormMissing ->
                invalidArgs ["FormMissing"]
            FormFailure errors ->
                invalidArgs errors

    runDB $ update noteId [NoteContent =. content]

    redirect $ NoteR noteId
