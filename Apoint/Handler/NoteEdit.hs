{-# LANGUAGE NamedFieldPuns
  #-}

module Handler.NoteEdit where

import Access
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

noteContentEditWidget :: Entity Note -> Handler Widget
noteContentEditWidget (Entity noteId note) = do
    let content = noteContent note
    (formWidget, enctype) <-
        generateFormPost $ noteContentEditForm (Just content)
    return [whamlet|
        <h1>Editing note
        <form method=post action=@{NoteR noteId} enctype=#{enctype}>
            ^{formWidget}
            <button>Submit
    |]


getNoteEditR :: NoteId -> Handler Html
getNoteEditR noteId = do
    note <- runDB $ get404 noteId
    authorize Read CurrentUser note
    w <- noteContentEditWidget (Entity noteId note)
    defaultLayout w


postNoteR :: NoteId -> Handler ()
postNoteR noteId = do
    note <- runDB $ get404 noteId
    authorize Access.Update CurrentUser note

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
