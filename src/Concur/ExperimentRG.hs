module Concur.ExperimentRG where

import SyntaxRG
import Concur.Effect ( ChannelT )
import Control.Concurrent.STM ( STM, atomically )
import Control.Concurrent.Async ( concurrently )
import Control.Concurrent.STM.TVar ( newTVarIO )
import System.Random ( random, StdGen, mkStdGen )

type Outcome = Bool
type Qsystem = ExprRG
type Observable = Qsystem -> ChannelT -> STM Outcome
-- temporarily fix the number of observables to 2
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

-- Execute both measurements concurrently
runContextT :: Qsystem -> Context -> IO (Outcome, Outcome)
runContextT qState (obsL, obsR) = do
    hVar <- newTVarIO (False, 0)  -- "initialize hidden variable"
    (resL, resR) <- 
        concurrently (atomically $ obsL qState hVar)
                     (atomically $ obsR qState hVar)
    return (resL, resR)

runContextsT :: [Qsystem] -> [Context] -> IO [(Outcome, Outcome)]
runContextsT qStates contexts = 
    sequence $ zipWith runContextT qStates contexts
