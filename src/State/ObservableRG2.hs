module State.ObservableRG2 where

import SyntaxRG ( ExprRG, BoolRG(R, G) )
import State.Effect ( M, M2 )
import State.ExperimentRG ( Outcome )
import State.InterpRG ( interpRG )
import Control.Monad.State.Lazy ( MonadState(put, get) )

-- with one bit + a natural number (or 3 bits) for covert communication
l1, l2, l3 :: ExprRG -> M2 Outcome
l1 e = do
    (b, _, _) <- interpRG e
    put (b, 1)
    return b
l2 e = do
    (_, b, _) <- interpRG e
    put (b, 2)
    return b
l3 e = do
    (_, _, b) <- interpRG e
    put (b, 3)
    return b

r1, r2, r3 :: ExprRG -> M2 Outcome
r1 e = do
    (b', n) <- get
    (b, _, _) <- interpRG e
    if b' == b then
        if n == 1 then return b
        else return (not b)
    else
        return b

r2 e = do
    (b', n) <- get
    (_, b, _) <- interpRG e
    if b' == b then
        if n == 2 then return b
        else return (not b)
    else
        return b

r3 e = do
    (b', n) <- get
    (_, _, b) <- interpRG e
    if b' == b then
        if n == 3 then return b
        else return (not b)
    else
        return b