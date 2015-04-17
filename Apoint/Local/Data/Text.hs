module Local.Data.Text where

import Control.Arrow  ( (>>>) )
import Data.Text      as Text
import Prelude        ( Show (show) )


show :: Show a => a -> Text
show = Prelude.show >>> Text.pack
