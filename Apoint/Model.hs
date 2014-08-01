module Model where

import Data.Text (Text)
import Data.Typeable (Typeable)
import Database.Persist.Quasi
import Prelude (Bool, Show)
import Yesod


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
