module State.Experiment where

import Syntax
import State.Effect ( M )
import System.Random
import Control.Monad.State.Lazy ( evalState )

-- experiment apparatus

type Outcome = Bool

-- expression as quantum system
type Qsystem = Expr
-- test as single observable
type Observable = Qsystem -> M Outcome
-- test suite as measurement context
type Context = [Observable]
-- joint outcomes given a context
type Experiment = Qsystem -> Context -> M [Outcome]

-- single observable outcome
exp1 :: Qsystem -> Observable -> M Outcome
exp1 expr f = f expr

-- joint outcomes of 2 observables
exp2 :: Qsystem -> (Observable, Observable) -> M (Outcome, Outcome)
exp2 expr (f1, f2) = do
    o1 <- f1 expr
    o2 <- f2 expr
    return (o1, o2)

-- joint outcomes of 3 observables
exp3 :: Qsystem 
    -> (Observable, Observable, Observable) 
    -> M (Outcome, Outcome, Outcome)
exp3 expr (f1, f2, f3) = do
    o1 <- f1 expr
    o2 <- f2 expr
    o3 <- f3 expr
    return (o1, o2, o3)

-- joint outcomes of 4 observables
exp4 :: Qsystem
    -> (Observable, Observable, Observable, Observable)
    -> M (Outcome, Outcome, Outcome, Outcome)
exp4 expr (f1, f2, f3, f4) = do
    o1 <- f1 expr
    o2 <- f2 expr
    o3 <- f3 expr
    o4 <- f4 expr
    return (o1, o2, o3, o4)

-- generalized joint outcomes given a context (list) of observables
-- joint outcomes depends on the order of observables...
expn :: Qsystem -> Context -> M [Outcome]
expn expr [] = return []
expn expr (f:fs) = do
    o <- f expr
    os <- expn expr fs
    return (o:os)


randomBool :: Int -> (Bool, StdGen)
randomBool seed = random (mkStdGen seed)

randomBoolStream :: Int -> [Bool]
randomBoolStream seed = randoms (mkStdGen seed)

type Seed = Int

-- reify the computational effect
runExperimentS :: Qsystem -> Context -> Seed -> [Outcome]
runExperimentS expr ctx seed =
    let m = expn expr ctx in
        evalState m (fst (randomBool seed))

printResultS :: Qsystem -> [Context] -> Seed -> IO ()
printResultS expr [] seed = pure ()
printResultS expr (c:cs) seed = do
    print (runExperimentS expr c seed)
    printResultS expr cs (seed + 1)