{-# LANGUAGE  ConstraintKinds
            , DeriveGeneric
            , NamedFieldPuns
  #-}

module Handler.Notes where

import Local.Yesod.Auth               (requireAuthId')

import Import


data NoteNew = NoteNew
    { nnContent :: Textarea
    }

noteNewForm :: Html -> MForm Handler (FormResult NoteNew, Widget)
noteNewForm = renderDivs $ NoteNew
    <$> areq textareaField "Content" Nothing

noteNewFormPage :: Widget -> Enctype -> Handler Html
noteNewFormPage widget enctype =
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
    noteNewFormPage widget enctype


getNotesR :: Handler Html
getNotesR = do
    userId <- requireAuthId'
    notes <- runDB $
        selectList
            [NoteAuthor ==. userId]
            [LimitTo $ notesOnAPage + 1] -- one for pagination
    defaultLayout $(widgetFile "notes")

    where
        notesOnAPage = 20


postNotesR :: Handler ()
postNotesR = do
    userId <- requireAuthId'

    ((formResult, _), _) <- runFormPost noteNewForm
    content <- case formResult of
        FormSuccess NoteNew{nnContent} -> return $ unTextarea nnContent
        FormMissing -> invalidArgs ["FormMissing"]
        FormFailure errors -> invalidArgs errors

    noteId <- runDB $ insert $ Note{noteContent = content, noteAuthor = userId}

    redirect $ NoteR noteId
