module State.ObservableRG where

import SyntaxRG
import State.Effect ( M )
import Control.Monad.State.Lazy ( MonadState(put, get) )


-- with one bit + a natural number (or 3 bits) for covert communication
-- by default, these observables can be interpreted as questions/predicates "is R?"
l1, l2, l3 :: ExprRG -> M Outcome
l1 (c1, _, _) = do
    (color, pos) <- get
    case (color, pos) of
        (Nothing, _) -> do
            put (Just c1, S1)
            return (c1 == R)
        (Just c, S1) -> 
            return (c1 == R)
        (Just c, _) -> 
            if c == c1 
                then return (c1 /= R) 
                else return (c1 == R)

l2 (_, c2, _) = do
    (color, pos) <- get
    case (color, pos) of
        (Nothing, _) -> do
            put (Just c2, S2)
            return (c2 == R)
        (Just c, S2) -> 
            return (c2 == R)
        (Just c, _) -> 
            if c == c2 
                then return (c2 /= R) 
                else return (c2 == R)

l3 (_, _, c3) = do
    (color, pos) <- get
    case (color, pos) of
        (Nothing, _) -> do
            put (Just c3, S3)
            return (c3 == R)
        (Just c, S3) -> 
            return (c3 == R)
        (Just c, _) -> 
            if c == c3 
                then return (c3 /= R) 
                else return (c3 == R)

r1, r2, r3 :: ExprRG -> M Outcome
r1 = l1
r2 = l2
r3 = l3