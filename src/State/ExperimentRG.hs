 {-# LANGUAGE UnicodeSyntax #-}
module State.ExperimentRG where

import SyntaxRG ( ExprRG, BoolRG )
import State.Effect ( M )
import State.ObservableRG3 ( l1, l2, l3, r1, r2, r3, Outcome )
import System.Random ( random, randoms, mkStdGen, StdGen )
import Control.Monad.State.Lazy ( evalState )

-- expression as quantum system
type Qsystem = ExprRG
-- test as single observable
type Observable = Qsystem -> M Outcome
-- test suite as measurement context
type Context = (Observable, Observable)
-- joint outcomes given a context
type Experiment = Qsystem -> Context -> M (Outcome, Outcome)

-- single observable outcome
exp1 :: Qsystem -> Observable -> M Outcome
exp1 expr f = f expr

-- joint outcomes of 2 observables
exp2 :: Qsystem -> Context -> M (Outcome, Outcome)
exp2 expr (f1, f2) = do
    o1 <- f1 expr
    o2 <- f2 expr
    return (o1, o2)

-- reify the computational effect per observable
run1 :: Qsystem -> Observable -> Outcome
run1 qState f =
    let m = exp1 qState f in
        evalState m (False, 0)

-- reify the computational effect per context
run2 :: Qsystem -> Context -> (Outcome, Outcome)
run2 qState ctx =
    let m = exp2 qState ctx in
        evalState m (False, 0)


-- contextual logical operators
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


p11, p12, p13 :: Observable
p11 = (l1 ⨂ r1) ∔ (nots l1 ⨂ nots r1) // expected to be True 100% of the time
p12 = (l1 ⨂ r2) ∔ (nots l1 ⨂ nots r2) // expected to be True 25% of the time
p13 = (l1 ⨂ r3) ∔ (nots l1 ⨂ nots r3) // expected to be True 25% of the time


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