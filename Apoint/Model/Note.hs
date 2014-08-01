module Model.Note where

import Control.Monad (forM)

import Model

import Import


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
