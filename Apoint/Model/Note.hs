module            Model.Note where

import            Control.Monad   ( forM )
import            Data.Default    ( def )
import            Data.Monoid     ( (<>) )
import qualified  Data.Text       as Text
import            Data.Text.Lazy  ( fromStrict )
import            Text.Markdown   ( markdown )

import            Import


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
