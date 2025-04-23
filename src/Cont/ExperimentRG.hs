module Cont.ExperimentRG where

import SyntaxRG
import Cont.Effect
import System.Random ( random, StdGen, mkStdGen )

type Qsystem = ExprRG
type Observable = Qsystem -> Iterator (Bool, Int) (Bool, Int) Outcome
type Context = (Observable, Observable)

-- Generate a random ExprRG "instruction set" (R/G,R/G,R/G)
genQState :: StdGen -> Qsystem
genQState gen =
    let (b1, g1) = random gen
        (b2, g2) = random g1
        (b3, _)  = random g2
    in (if b1 then R else G, if b2 then R else G, if b3 then R else G)

genQStates :: Int -> [Qsystem]
genQStates 0 = []
genQStates n = genQState gen : genQStates (n - 1)
    where gen = mkStdGen n


runSingletonC :: Qsystem -> (Observable) -> IO (Outcome)
runSingletonC qState (obs) = do
    let Susp (b, n) k = obs qState in
        return b


runContextC :: Qsystem -> Context -> IO (Outcome, Outcome)
runContextC qState (obsL, obsR) =
    let Susp (bL, nL) kL = obsL qState
        Susp (bR, nR) kR = obsR qState in do
        -- only one observable should be modified
        let Result bL' = kL (bR, nR)
            Result bR' = kR (bL, nL) in do
                -- (bL', bR') will cancel out the effect
                return (bL, bR')

-- run multiple pairs of Qsystems and Contexts
runContextsC :: [Qsystem] -> [Context] -> IO [(Outcome, Outcome)]
runContextsC qStates contexts = 
    sequence $ zipWith runContextC qStates contexts







