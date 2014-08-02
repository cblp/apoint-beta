module Model where

import Data.Text (Text)
import Data.Text as Text (concat)
import Data.Typeable (Typeable)
import Database.Persist.Quasi
import Prelude (Bool, Either(Left), Show, ($))
import Yesod


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


contains_i :: EntityField record Text -> Text -> Filter record
contains_i field val =
    Filter
        field
        (Left $ Text.concat ["%", val, "%"])
        (BackendSpecificFilter "LIKE") -- TODO: ILIKE for Postgres
