module State.Effect where

import SyntaxRG
import Control.Monad.State.Lazy

type M = State (RGU, Position)
type MU = State ExprRGU