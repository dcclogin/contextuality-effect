module Cont.Experiment where

import Syntax
import Cont.Effect ( M )
import Control.Monad.Cont
import System.Random

-- experiment apparatus

type Outcome = Bool

-- expression as quantum system
type Qsystem = Expr
-- test as single observable
type Observable = (Qsystem, Outcome) -> M Outcome
-- test suite as measurement context
type Context = [Observable]
-- joint outcomes given a context
type Experiment = Qsystem -> Context -> M [Outcome]


randomBool :: Int -> (Bool, StdGen)
randomBool seed = random (mkStdGen seed)

randomBoolStream :: Int -> [Bool]
randomBoolStream seed = randoms (mkStdGen seed)

type Seed = Int

-- generalized joint outcomes given a context (list) of observables
-- joint outcomes depends on the order of observables...
expn :: (Qsystem, Outcome) -> Context -> M [Outcome]
expn (expr, i) [] = return []
expn (expr, i) (f:fs) = do
    o <- f (expr, i)
    os <- expn (expr, o) fs
    return (o:os)


-- reify the computational effect
-- runExperimentC :: Qsystem -> Context -> (Bool -> Bool) -> [Outcome]
