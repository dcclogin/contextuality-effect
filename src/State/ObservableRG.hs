module State.ObservableRG where

import SyntaxRG ( ExprRG, BoolRG(R, G) )
import State.Effect ( M )
import State.InterpRG ( interpRG )
import Control.Monad.State.Lazy ( MonadState(put, get) )

-- 6 observables:
-- 3 for the left receiver, 3 for the right receiver
type Outcome = Bool

-- vanilla "instruction set" model, effectless
l1, l2, l3 :: ExprRG -> M Outcome
l1 e = do
    (b, _, _) <- interpRG e
    return b
l2 e = do
    (_, b, _) <- interpRG e
    return b
l3 e = do
    (_, _, b) <- interpRG e
    return b

r1, r2, r3 :: ExprRG -> M Outcome
r1 e = do
    (b, _, _) <- interpRG e
    return b
r2 e = do
    (_, b, _) <- interpRG e
    return b
r3 e = do
    (_, _, b) <- interpRG e
    return b

-- with just a bit for covert communication?

