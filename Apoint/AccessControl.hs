module AccessControl where

import Control.Monad (when)

import Import


checkUserCanRead :: UserId -> Note -> Handler ()
checkUserCanRead userId note =
    when (userId /= noteAuthor note) $
        permissionDenied "You are not the author of this note"


checkUserCanWrite :: UserId -> Note -> Handler ()
checkUserCanWrite = checkUserCanRead
