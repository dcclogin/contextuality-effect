module State.Effect where

import SyntaxRG
import System.Random (StdGen)
import Control.Monad.State.Lazy (State)

-- alternative probably: nested state monad, StdGen as argument
type M = State (RGU, Position, StdGen)
type MU = State (ExprRGU, StdGen)