module Prelude.Extended ( module I
                        , module Prelude
                        , module Prelude.Extended
                        ) where

import Control.Applicative        as I  ( (<$>), (<*>) )
import Control.Monad              as I  ( forM, when )
import Control.Monad.IO.Class     as I  ( liftIO )
import Control.Monad.Loops        as I  ( untilM_ )
import Control.Monad.Trans.Class  as I  ( lift )
import Data.Default               as I  ( def )
import Data.Maybe                 as I  ( fromMaybe, isJust )
import Data.Monoid                as I  ( (<>), mconcat )
import Data.Text                  as I  ( Text )
import Prelude


(&) :: a -> (a -> b) -> b
x & f = f x
{-# INLINE (&) #-}


curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry3 f a b c = f (a, b, c)
