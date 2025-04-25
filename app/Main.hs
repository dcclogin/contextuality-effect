module Main (main) where

import Pure.ExperimentRG
import State.ExperimentRG
import Concur.ExprRGConcur
import Cont.ExprRGCont
import System.Random (mkStdGen)

main :: IO ()
main = do
    Pure.ExperimentRG.runExperiment 100000 (mkStdGen 24)
    State.ExperimentRG.runExperiment 100000 (mkStdGen 42)
    Concur.ExprRGConcur.printRun 10000 88888
    Cont.ExprRGCont.printRun 10000 99999