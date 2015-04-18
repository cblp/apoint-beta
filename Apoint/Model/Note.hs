module            Model.Note where

import qualified  Data.Text         as Text
import            Data.Text         ( Text )
import            Data.Text.Lazy    ( fromStrict )
import            Prelude.Extended
import            Text.Blaze.Html   ( Html )
import            Text.Markdown     ( markdown )
import            Yesod.Persist     ( Entity (..), Filter
                                    , entityVal, get, runDB, selectList
                                    )

import            Foundation        ( Handler )
import            Model             ( Note (..), NoteId
                                    , Notelink
                                    )


noteContentHtml :: Note -> Html
noteContentHtml = markdown def . fromStrict . noteContent


noteContentShort :: Note -> Text
noteContentShort note =
    case Text.lines $ noteContent note of
        []           -> "..."
        firstLine:[] -> firstLine
        firstLine:_  -> firstLine <> "..."


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
