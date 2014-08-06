module Handler.Link where

import            Data.Char (isDigit)
import qualified  Data.Text as Text
import            Yesod.Form.Jquery

import            Import


noteLinkForm :: Html -> MForm Handler (FormResult Text, Widget)
noteLinkForm = renderDivsNoLabels $
    areq (jqueryAutocompleteField SearchSuggestR) "" Nothing


createLink :: NoteId -> NoteId -> Handler ()
createLink noteIdFrom noteIdTo = runDB $ do
    checkExists noteIdFrom
    checkExists noteIdTo
    insertUnique $ Notelink noteIdFrom noteIdTo
    >> return ()

    where
        checkExists key = do
            c <- count [NoteId ==. key]
            case c of
                0 -> lift $ invalidArgs ["bad note " <> toPathPiece key]
                1 -> return ()
                _ -> lift $ invalidArgs ["bad count for noteId"]


getNoteIdOutOfLinkForm :: Handler NoteId
getNoteIdOutOfLinkForm = do
    ((formResult, _), _) <- runFormPost noteLinkForm
    noteSelector <- case formResult of
        FormMissing               -> invalidArgs ["FormMissing"]
        FormFailure errors        -> invalidArgs errors
        FormSuccess noteSelector  -> return noteSelector
    -- getting last digit cluster
    let noteIdList =  noteSelector
                      |> Text.split (not . isDigit)
                      |> filter (not . Text.null)
    noteIdMaybe <- if null noteIdList
        then invalidArgs ["cannot find noteId"]
        else return $ fromPathPiece $ last noteIdList
    case noteIdMaybe of
        Nothing     -> invalidArgs ["bad noteId"]
        Just noteId -> return noteId


postLinkFromCreateR :: NoteId -> Handler ()
postLinkFromCreateR noteIdFrom = do
    noteIdTo <- getNoteIdOutOfLinkForm
    createLink noteIdFrom noteIdTo
    redirect $ NoteR noteIdFrom


postLinkToCreateR :: NoteId -> Handler ()
postLinkToCreateR noteIdTo = do
    noteIdFrom <- getNoteIdOutOfLinkForm
    createLink noteIdFrom noteIdTo
    redirect $ NoteR noteIdTo
