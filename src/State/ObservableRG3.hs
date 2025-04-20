module State.ObservableRG3 where

import SyntaxRG ( ExprRG, BoolRG(R, G) )
import State.Effect ( M )
import State.InterpRG ( interpRG )
import Control.Monad.State.Lazy ( MonadState(put, get) )

type Outcome = Bool

-- with one bit + a natural number (or 3 bits) for covert communication
-- by default, these observables can be interpreted as questions/predicates "is R?"
l1, l2, l3 :: ExprRG -> M Outcome
l1 e = do
    (b1, _, _) <- interpRG e
    (b, n) <- get
    if n == 0 then do
        put (b1, 1)
        return b1
    else if n == 1 then
        return b1
    else if b1 == b then
        return (not b1)
    else
        return b1
l2 e = do
    (_, b2, _) <- interpRG e
    (b, n) <- get
    if n == 0 then do
        put (b2, 2)
        return b2
    else if n == 2 then
        return b2
    else if b2 == b then
        return (not b2)
    else
        return b2
l3 e = do
    (_, _, b3) <- interpRG e
    (b, n) <- get
    if n == 0 then do
        put (b3, 3)
        return b3
    else if n == 3 then
        return b3
    else if b3 == b then
        return (not b3)
    else
        return b3

r1, r2, r3 :: ExprRG -> M Outcome
r1 = l1
r2 = l2
r3 = l3