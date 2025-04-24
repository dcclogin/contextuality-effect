 {-# LANGUAGE UnicodeSyntax #-}
module State.ExperimentRG where

import SyntaxRG
import System.Random
import State.Effect ( M )
import State.ObservableRG ( l1, l2, l3, r1, r2, r3 )
import Control.Monad.State.Lazy ( evalState )

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
run1 :: Qsystem -> Observable -> Outcome
run1 qState f =
    let m = exp1 qState f in
        evalState m (Nothing, S1)

-- reify the computational effect per context
run2 :: Qsystem -> Context -> (Outcome, Outcome)
run2 qState ctx =
    let m = exp2 qState ctx in
        evalState m (Nothing, S1)


-- contextual logical operators (?)
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
p13 = (l1 ⨂ r3) ∔ (nots l1 ⨂ nots r3) -- expected to be True 25% of the time



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

type Query = (Outcome, Outcome) -> M Outcome

psc :: Context -> Qsystem -> M Outcome
psc ctx qState =
    let m = exp2 qState ctx in
        m >>= \(o1, o2) -> return ((o1 && o2) || (not o1 && not o2))



{- randomBool :: Int -> (Bool, StdGen)
randomBool seed = random (mkStdGen seed)

randomBoolStream :: Int -> [Bool]
randomBoolStream seed = randoms (mkStdGen seed) -}

--type Seed = Int

{- -- reify the computational effect per experiment
runExperimentS :: Qsystem -> [Context] -> Seed -> [[Outcome]]
runExperimentS expr cs seed =
    let m = expnn expr cs in
        evalState m (fst (randomBool seed), 0)

-- connecting contexts with progression (seed + 1)
runContextsS :: Qsystem -> [Context] -> Seed -> [[Outcome]]
runContextsS expr [] seed = []
runContextsS expr (c:cs) seed =
    let os = runContextS expr c seed in
        let oss = runContextsS expr cs (seed + 1) in
            (os:oss) -}


