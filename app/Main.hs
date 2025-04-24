module Main (main) where

import State.ExprRGState
import Concur.ExprRGConcur
import Cont.ExprRGCont
import System.Random (mkStdGen)

main :: IO ()
main = do
    State.ExprRGState.runExperiment 100000 (mkStdGen 111)
    Concur.ExprRGConcur.printRun 10000 88888
    Cont.ExprRGCont.printRun 10000 99999