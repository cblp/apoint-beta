module Handler.Admin.User where

import Prelude
import Text.Blaze.Html    ( Html )
import Yesod.Core         ( defaultLayout )
import Yesod.Core.Widget  ( whamlet )
import Yesod.Persist      ( Entity(..), runDB, selectList )

import Access     ( authorizeAdmin )
import Foundation -- Handler
import Model      -- User


getAdminUsersR :: Handler Html
getAdminUsersR = do
    authorizeAdmin
    users :: [Entity User] <- runDB $ selectList [] []
    defaultLayout [whamlet|
        <table>
            $forall Entity _ user <- users
                <tr>
                    <td>
                        #{show user}
    |]
