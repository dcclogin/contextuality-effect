module State.Observable where

import Syntax ( Expr, Value(PairV, Error, BoolV) )
import State.Effect ( M )
import State.Experiment ( Outcome )
import State.Interp ( andM, interpM, orM, pairVReducer )
import Control.Monad.State.Lazy ( MonadState(put, get) )


-- examples of observables as test cases
-- protocols of observation/measurement
-- simulating empirical synthesis process

pureContingent :: Expr -> M Outcome
pureContingent expr = get

pureContingentNot :: Expr -> M Outcome
pureContingentNot expr = do
    v <- get
    return (not v)

contingentOnInterp :: Expr -> M Outcome
contingentOnInterp expr = do
    v <- interpM [] expr
    case v of
        BoolV b -> return b
        _ -> get

contingentOnInterpAnd :: Expr -> M Outcome
contingentOnInterpAnd expr = do
    v <- interpM [] expr
    b1 <- get
    case v of
        BoolV b2 -> return (b1 && b2)
        _ -> get

contingentOnInterpOr :: Expr -> M Outcome
contingentOnInterpOr expr = do
    v <- interpM [] expr
    b1 <- get
    case v of
        BoolV b2 -> return (b1 || b2)
        _ -> get

contingentOnReducerOr :: Expr -> M Outcome
contingentOnReducerOr expr = do
    v <- interpM [] expr
    v <- pairVReducer orM v
    case v of
        BoolV b -> return b
        _ -> get

contingentOnReducerAnd :: Expr -> M Outcome
contingentOnReducerAnd expr = do
    v <- interpM [] expr
    v <- pairVReducer andM v
    case v of
        BoolV b -> return b
        _ -> get

causeTrue :: Expr -> M Outcome
causeTrue expr = do
    put True
    contingentOnInterp expr

causeFalse :: Expr -> M Outcome
causeFalse expr = do
    put False
    contingentOnInterp expr

causeId :: Expr -> M Outcome
causeId expr = do
    v <- get
    put v
    return v

causeNot :: Expr -> M Outcome
causeNot expr = do
    v <- get
    put (not v)
    return v

causeNot2 :: Expr -> M Outcome
causeNot2 expr = do
    causeNot expr

causeNotReturnNot :: Expr -> M Outcome
causeNotReturnNot expr = do
    v <- get
    put (not v)
    return (not v)

hasError :: Expr -> M Outcome
hasError expr = do
    v <- interpM [] expr
    case v of
        Error -> return True
        _ -> return False

isBoolV :: Expr -> M Outcome
isBoolV expr = do
    v <- interpM [] expr
    case v of
        BoolV b -> return True
        _ -> return False 

isNotPair :: Expr -> M Outcome
isNotPair expr = do
    v <- interpM [] expr
    case v of
        PairV _ _ -> return False
        _ -> return True
