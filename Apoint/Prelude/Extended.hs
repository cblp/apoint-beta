module Prelude.Extended     ( module Control.Applicative
                            , module Prelude
                            , module Prelude.Extended
                            ) where

import Control.Applicative  ( (<$>), (<*>) )
import Prelude


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)
