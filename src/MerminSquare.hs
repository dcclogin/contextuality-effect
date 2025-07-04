module MerminSquare where

-- prototyping Mermin square

import Data.Matrix
import Control.Monad.State.Lazy

type Decision = Bool

data PropertyP = P1 | P2 | P3
  deriving (Eq, Show)

data PropertyQ = Q1 | Q2 | Q3
  deriving (Eq, Show)

type System f m = f (PropertyP -> PropertyQ -> m Decision)

-- a row is allowed to have odd number of True
-- a column is allowed to have even number of True
rowProperty, colProperty :: Decision -> Decision -> Decision -> Bool
rowProperty d1 d2 d3 = if d1 then d2 == d3 else d2 /= d3
colProperty d1 d2 d3 = if d1 then d2 /= d3 else d2 == d3


type M = StateT (Matrix (Maybe Decision)) IO


sys :: PropertyP -> PropertyQ -> M Decision
sys p q = return True