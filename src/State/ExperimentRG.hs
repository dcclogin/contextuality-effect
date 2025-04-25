 {-# LANGUAGE UnicodeSyntax #-}
module State.ExperimentRG where

import SyntaxRG
import System.Random
import RandomUtils
import State.Effect ( M )
import State.ObservableRG ( l1, l2, l3, r1, r2, r3 )
import Control.Monad.State.Lazy ( evalState, runState )

-- expression as quantum system
type Qsystem = ExprRG
-- test as single observable
type Observable = Qsystem -> M Outcome
-- test suite as measurement context
type Context = (Observable, Observable)
-- joint outcomes given a context
type Experiment = Qsystem -> Context -> M (Outcome, Outcome)

-- outcome of a single observable
exp1 :: Qsystem -> Observable -> M Outcome
exp1 qState f = f qState

-- joint outcomes of 2 observables
exp2 :: Qsystem -> Context -> M (Outcome, Outcome)
exp2 qState (f1, f2) = do
    o1 <- f1 qState
    o2 <- f2 qState
    return (o1, o2)

-- reify the computational effect per observable
run1 :: StdGen -> Qsystem -> Observable -> (Outcome, StdGen)
run1 gen qState f =
    let m = exp1 qState f
        (val, (_, _, g)) = runState m (Nothing, S1, gen)
    in (val, g)

-- reify the computational effect per context
run2 :: StdGen -> Qsystem -> Context -> ((Outcome, Outcome), StdGen)
run2 gen qState ctx =
    let m = exp2 qState ctx
        (val, (_, _, g)) = runState m (Nothing, S1, gen)
    in (val, g)

type Query = (Outcome, Outcome) -> M Outcome

-- constant queries
tt, ff :: Query
tt = \_ -> return True
ff = \_ -> return False

-- omit one of the outcomes
lo, ro :: Query
lo = \(o1, o2) -> return o1
ro = \(o1, o2) -> return o2

-- logical operation for queries
(⨂) :: Query -> Query -> Query
f1 ⨂ f2 = \(o1, o2) -> do
    r1 <- f1 (o1, o2)
    r2 <- f2 (o1, o2)
    return (r1 && r2)

(⨁) :: Query -> Query -> Query
f1 ⨁ f2 = \(o1, o2) -> do
    r1 <- f1 (o1, o2)
    r2 <- f2 (o1, o2)
    return (r1 || r2)

notq :: Query -> Query
notq f = \(o1, o2) -> do
    r <- f (o1, o2)
    return (not r)

q1 :: Query
q1 = (lo ⨂ ro) ⨁ (notq lo ⨂ notq ro)


{- psc :: Context -> Qsystem -> M Outcome
psc ctx qState =
    let m = exp2 qState ctx in
        m >>= \(o1, o2) -> return ((o1 && o2) || (not o1 && not o2)) -}

configToContext :: Config -> Context
configToContext config = case config of
    (S1, S1) -> (l1, r1)
    (S1, S2) -> (l1, r2)
    (S1, S3) -> (l1, r3)
    (S2, S1) -> (l2, r1)
    (S2, S2) -> (l2, r2)
    (S2, S3) -> (l2, r3)
    (S3, S1) -> (l3, r1)
    (S3, S2) -> (l3, r2)
    (S3, S3) -> (l3, r3)

-- execute with the record of the configuration/context
exec :: StdGen -> Qsystem -> Config -> (Record2O, StdGen)
exec gen qState config = 
    let ctx = configToContext config
        (res, g) = run2 gen qState ctx
    in ((config, res), g)

-- multiple executions with records
execl :: StdGen -> [Qsystem] -> [Config] -> ([Record2O], StdGen)
execl gen [] _ = ([], gen)
execl gen (e:es) (c:cs) =
    let (r, g1) = exec gen e c
        (rs, g2) = execl g1 es cs
    in (r:rs, g2)

-- execute with one customized question (regarding original outcomes)
execq :: StdGen -> Query -> Qsystem -> Config -> (Record1O, StdGen)
execq gen query qState config =
    let ctx = configToContext config
        m = exp2 qState ctx >>= query
        (res, (_, _, g)) = runState m (Nothing, S1, gen)
    in ((config, res), g)

-- multiple executions with records
execql :: StdGen -> Query -> [Qsystem] -> [Config] -> ([Record1O], StdGen)
execql gen query [] _ = ([], gen)
execql gen query (e:es) (c:cs) =
    let (r, g1) = execq gen query e c
        (rs, g2) = execql g1 query es cs
    in (r:rs, g2)

runQuery :: Int -> StdGen -> Query -> [Record1O]
runQuery n gen query =
    let (es, g1) = genExprRGs n gen
        (cs, g2) = genConfigs n g1
        (rs, _) = execql g2 query es cs
    in rs

runExperiment :: Int -> StdGen -> IO ()
runExperiment n gen = do
    -- "eager generation"
    let (es, g1) = genExprRGs n gen
        (cs, g2) = genConfigs n g1
        (s1, d1, s2, d2) = allStats (fst (execl g2 es cs)) in do
            putStrLn "\nMeasurement results (State)"
            putStrLn $ "{XX (pos1 == pos2, 3)} {RR,GG}: " ++ show s1
            putStrLn $ "{XX (pos1 == pos2, 3)} {RG,GR}: " ++ show d1
            putStrLn $ "{XY (pos1 != pos2, 6)} {RR,GG}: " ++ show s2
            putStrLn $ "{XY (pos1 != pos2, 6)} {RG,GR}: " ++ show d2
            putStrLn $ "Total number of records: " ++ show n







{- -- contextual logical operators (?)
notc :: Observable -> Observable
notc f = \qState -> do
    o <- f qState
    return (not o)

(⨂) :: Observable -> Observable -> Observable
f1 ⨂ f2 = \qState -> do
    o1 <- f1 qState
    o2 <- f2 qState
    return (o1 && o2)

(⨁) :: Observable -> Observable -> Observable
f1 ⨁ f2 = \qState -> do
    o1 <- f1 qState
    o2 <- f2 qState
    return (o1 || o2)

-- non-contextual logical operators
nots :: Observable -> Observable
nots f = \qState -> do
    let o = run1 qState f in
        return (not o)

(⨰) :: Observable -> Observable -> Observable
f1 ⨰ f2 = \qState -> do
    let o1 = run1 qState f1
        o2 = run1 qState f2 in
            return (o1 && o2)

(∔) :: Observable -> Observable -> Observable
f1 ∔ f2 = \qState -> do
    let o1 = run1 qState f1
        o2 = run1 qState f2 in
            return (o1 || o2)

tt, ff :: Observable
tt = \_ -> return True
ff = \_ -> return False


-- it is problematic, since the context is executed twice!
p11, p12, p13 :: Observable
p11 = (l1 ⨂ r1) ∔ (nots l1 ⨂ nots r1) -- expected to be True 100% of the time
p12 = (l1 ⨂ r2) ∔ (nots l1 ⨂ nots r2) -- expected to be True 25% of the time
p13 = (l1 ⨂ r3) ∔ (nots l1 ⨂ nots r3) -- expected to be True 25% of the time -}



-- (>>=) :: M a -> (a -> M b) -> M b

{- type Nil = M Outcome
type Una = Outcome -> M Outcome
type Due = Outcome -> Outcome -> M Outcome
type Duo = (Outcome, Outcome) -> M Outcome

idm :: Una
idm = \o -> return o

notm :: Una
notm = \o -> return (not o)

andm :: Duo
andm = \(o1, o2) -> return (o1 && o2)

andn :: Una -> Una -> Duo
f1 `andn` f2 = \(o1, o2) -> return (f1 o1 && f2 o2)


-- predicate "is same color?"
psc :: Context -> Qsystem -> M Outcome
psc ctx qState = do
    let m = exp2 qState ctx in
        m >>= \(o1, o2) -> return (o1 && o2) || (not o1 && not o2) -}

-- what about "is not same color?"
-- how to make these predicates more compositional?