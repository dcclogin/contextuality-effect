module State.Interp where

import Syntax
import State.Effect ( M )

lookupM :: Name -> Env -> M Value
lookupM s [] = return Error
lookupM s ((x,v):rest) = if (x == s) then (return v) else (lookupM s rest)

notM :: Value -> M Value
notM (BoolV b) = return (BoolV (not b))
notM _ = return Error

orM, andM :: Value -> Value -> M Value
orM (BoolV b1) (BoolV b2) = return (BoolV (b1 || b2))
orM _ _ = return Error

andM (BoolV b1) (BoolV b2) = return (BoolV (b1 && b2))
andM _ _ = return Error

interpM :: Env -> Expr -> M Value
interpM env (Var s) = lookupM s env
interpM env (Const b) = return (BoolV b)
interpM env (NotE e) = do
    v <- interpM env e
    notM v
interpM env (OrE e1 e2) = do
    v1 <- interpM env e1
    v2 <- interpM env e2
    orM v1 v2
interpM env (AndE e1 e2) = do
    v1 <- interpM env e1
    v2 <- interpM env e2
    andM v1 v2
interpM env (PairE e1 e2) = do
    v1 <- interpM env e1
    v2 <- interpM env e2
    return (PairV v1 v2)
interpM env (Fst e) = do
    v <- interpM env e
    case v of
        PairV v1 v2 -> return v1
        _ -> return Error
interpM env (Snd e) = do
    v <- interpM env e
    case v of
        PairV v1 v2 -> return v2
        _ -> return Error
interpM env (LetE x e1 e2) = do
    v1 <- interpM env e1
    interpM ((x,v1):env) e2


pairVReducer :: (Value -> Value -> M Value) -> Value -> M Value
pairVReducer binop (BoolV b) = return (BoolV b)
pairVReducer binop (PairV v1 v2) = do
    v1' <- pairVReducer binop v1
    v2' <- pairVReducer binop v2
    binop v1' v2'
pairVReducer binop Error = return Error