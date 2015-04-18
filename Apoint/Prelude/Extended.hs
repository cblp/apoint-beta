module Prelude.Extended     ( module Import
                            , module Prelude
                            , module Prelude.Extended
                            ) where

import Control.Applicative  as Import ( (<$>), (<*>) )
import Data.Default         as Import ( def )
import Prelude


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)
