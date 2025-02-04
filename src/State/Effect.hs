module State.Effect where

import Control.Monad.State.Lazy

type M = State Bool
type M2 = State (Bool, Int)