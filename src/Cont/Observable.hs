module Cont.Observable where

import Syntax
import Cont.Effect
import Cont.Experiment ( Outcome )
import Cont.Interp
import Control.Monad.Cont


negate :: (Expr, Outcome) -> M Outcome
negate (expr, o) = do
    v <- interpM [] expr
    case v of
        BoolV b -> return (not b)
        _ -> return o

negate2 :: (Expr, Outcome) -> M Outcome
negate2 (expr, o) = do
    v <- interpM [] expr
    case v of
        BoolV b -> return (not b)
        _ -> return (not o)

preserve :: (Expr, Outcome) -> M Outcome
preserve (expr, o) = do
    v <- interpM [] expr
    case v of
        BoolV b -> return b
        _ -> return o

andC :: (Expr, Outcome) -> M Outcome
andC (expr, o) = do
    v <- interpM [] expr
    case v of
        BoolV b -> return (b && o)
        _ -> return o

orC :: (Expr, Outcome) -> M Outcome
orC (expr, o) = do
    v <- interpM [] expr
    case v of
        BoolV b -> return (b || o)
        _ -> return o