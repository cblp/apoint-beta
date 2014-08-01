module Handler.Search where

import Import


getSearchR :: Handler Html
getSearchR = do
    defaultLayout [whamlet|
        <h1>Search
        <h2>
    |]
