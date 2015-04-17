module Handler.Link where

import            Control.Lens      ( (&) )
import            Data.Char         ( isDigit )
import            Data.Monoid       ( (<>) )
import qualified  Data.Text         as Text
import            Data.Text         ( Text )
import            Prelude
import            Yesod.Form.Jquery ( jqueryAutocompleteField )

import            Form              ( runFormPostChecked )
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
    noteSelector <- runFormPostChecked noteLinkForm
    -- getting last digit cluster
    let noteIdList =  noteSelector
                      & Text.split (not . isDigit)
                      & filter (not . Text.null)
    noteIdMaybe <- if null noteIdList
        then invalidArgs ["cannot find noteId"]
        else return $ fromPathPiece $ last noteIdList
    case noteIdMaybe of
        Nothing     -> invalidArgs ["bad noteId"]
        Just noteId -> return noteId


postLinkCreateR :: Rel -> NoteId -> Handler ()
postLinkCreateR rel thisNoteId = do
    thatNoteId <- getNoteIdOutOfLinkForm
    let (nFrom, nTo) = case rel of
            RelFrom -> (thisNoteId, thatNoteId)
            RelTo   -> (thatNoteId, thisNoteId)
    createLink nFrom nTo
    redirect $ NoteR thisNoteId
