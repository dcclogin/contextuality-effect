module State.ObservableRG where

import SyntaxRG
import RandomUtils
import System.Random
import State.Effect ( M )
import Control.Monad.State.Lazy ( MonadState(put, get) )


-- these observables can be interpreted as questions/predicates "is R?"
l1, l2, l3 :: ExprRG -> M Outcome
l1 (c1, _, _) = do
    (color, pos, gen) <- get
    case (color, pos) of
        (Nothing, _) -> do
            put (Just c1, S1, gen)
            return (c1 == R)
        (Just c, S1) ->
            return (c1 == R)
        (Just c, _) ->
            if c == c1
            then let (b, g) = genTrue 1 2 gen in do
                put (color, pos, g)
                return (if b then (c1 == R) else (c1 /= R)) 
            else return (c1 == R)

l2 (_, c2, _) = do
    (color, pos, gen) <- get
    case (color, pos) of
        (Nothing, _) -> do
            put (Just c2, S2, gen)
            return (c2 == R)
        (Just c, S2) -> 
            return (c2 == R)
        (Just c, _) -> 
            if c == c2 
            then let (b, g) = genTrue 1 2 gen in do
                put (color, pos, g)
                return (if b then (c2 == R) else (c2 /= R))
            else return (c2 == R)

l3 (_, _, c3) = do
    (color, pos, gen) <- get
    case (color, pos) of
        (Nothing, _) -> do
            put (Just c3, S3, gen)
            return (c3 == R)
        (Just c, S3) -> 
            return (c3 == R)
        (Just c, _) -> 
            if c == c3 
            then let (b, g) = genTrue 1 2 gen in do
                put (color, pos, g)
                return (if b then (c3 == R) else (c3 /= R))
            else return (c3 == R)

r1, r2, r3 :: ExprRG -> M Outcome
r1 = l1
r2 = l2
r3 = l3