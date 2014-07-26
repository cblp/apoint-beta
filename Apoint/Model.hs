module Model where

import Control.Arrow ((>>>))
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text, lines)
import Data.Text.Lazy (fromStrict)
import Data.Typeable (Typeable)
import Database.Persist.Quasi
import Prelude (Bool, Show, ($))
import Text.Markdown (markdown)
import Yesod


-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")


noteContentHtml :: Note -> Html
noteContentHtml = noteContent >>> fromStrict >>> markdown def


noteContentShort :: Note -> Text
noteContentShort note =
    case lines $ noteContent note of
        []           -> "..."
        firstLine:[] -> firstLine
        firstLine:_  -> firstLine <> "..."
