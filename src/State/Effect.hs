module State.Effect where

import Control.Monad.State.Lazy

type M0 = State Bool
type M = State (Bool, Int)