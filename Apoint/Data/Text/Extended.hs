module Data.Text.Extended where

import Data.Text as Text
import Prelude


show :: Show a => a -> Text
show = Text.pack . Prelude.show
