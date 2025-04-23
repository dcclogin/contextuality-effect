module State.Observable where

import Syntax ( Expr, Value(PairV, Error, BoolV) )
import State.Effect ( M0 )
import State.Experiment ( Outcome )
import State.Interp ( andM, interpM, orM, pairVReducer )
import Control.Monad.State.Lazy ( MonadState(put, get) )


-- examples of observables as test cases
-- protocols of observation/measurement
-- simulating empirical synthesis process

pureContingent :: Expr -> M0 Outcome
pureContingent expr = get

pureContingentNot :: Expr -> M0 Outcome
pureContingentNot expr = do
    v <- get
    return (not v)

contingentOnInterp :: Expr -> M0 Outcome
contingentOnInterp expr = do
    v <- interpM [] expr
    case v of
        BoolV b -> return b
        _ -> get

contingentOnInterpAnd :: Expr -> M0 Outcome
contingentOnInterpAnd expr = do
    v <- interpM [] expr
    b1 <- get
    case v of
        BoolV b2 -> return (b1 && b2)
        _ -> get

contingentOnInterpOr :: Expr -> M0 Outcome
contingentOnInterpOr expr = do
    v <- interpM [] expr
    b1 <- get
    case v of
        BoolV b2 -> return (b1 || b2)
        _ -> get

contingentOnReducerOr :: Expr -> M0 Outcome
contingentOnReducerOr expr = do
    v <- interpM [] expr
    v <- pairVReducer orM v
    case v of
        BoolV b -> return b
        _ -> get

contingentOnReducerAnd :: Expr -> M0 Outcome
contingentOnReducerAnd expr = do
    v <- interpM [] expr
    v <- pairVReducer andM v
    case v of
        BoolV b -> return b
        _ -> get

causeTrue :: Expr -> M0 Outcome
causeTrue expr = do
    put True
    contingentOnInterp expr

causeFalse :: Expr -> M0 Outcome
causeFalse expr = do
    put False
    contingentOnInterp expr

causeId :: Expr -> M0 Outcome
causeId expr = do
    v <- get
    put v
    return v

causeNot :: Expr -> M0 Outcome
causeNot expr = do
    v <- get
    put (not v)
    return v

causeNot2 :: Expr -> M0 Outcome
causeNot2 expr = do
    causeNot expr

causeNotReturnNot :: Expr -> M0 Outcome
causeNotReturnNot expr = do
    v <- get
    put (not v)
    return (not v)

hasError :: Expr -> M0 Outcome
hasError expr = do
    v <- interpM [] expr
    case v of
        Error -> return True
        _ -> return False

isBoolV :: Expr -> M0 Outcome
isBoolV expr = do
    v <- interpM [] expr
    case v of
        BoolV b -> return True
        _ -> return False 

isNotPair :: Expr -> M0 Outcome
isNotPair expr = do
    v <- interpM [] expr
    case v of
        PairV _ _ -> return False
        _ -> return True
