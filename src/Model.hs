module            Model where

import qualified  Data.Text               as Text
import            Data.Typeable           ( Typeable )
import            Database.Persist        ( PersistFilter
                                            ( BackendSpecificFilter )
                                          , EntityField
                                          , Filter (Filter)
                                          )
import            Database.Persist.Quasi  ( lowerCaseSettings )
import            Database.Persist.TH     ( mkMigrate
                                          , mkPersist
                                          , persistFileWith
                                          , share
                                          , sqlOnlySettings
                                          )
import            Prelude.Extended


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
        (BackendSpecificFilter "ILIKE")
