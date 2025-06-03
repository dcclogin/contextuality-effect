module Main (Main.main) where

import Pure.ExperimentRG
import State.ExperimentRG
import Concur.ExprRGConcur
import Cont.ExprRGCont
import MerminCubes
import ManufacturingQC
import PaperReview
import State.PaperForget
import State.PaperNothing
import System.Random (mkStdGen)

main :: IO ()
main = do
    -- Pure.ExperimentRG.runExperiment 100000 (mkStdGen 24)
    -- State.ExperimentRG.runExperiment 100000 (mkStdGen 42)
    -- Concur.ExprRGConcur.printRun 10000 88888
    -- Cont.ExprRGCont.printRun 10000 99999
    -- MerminCubes.main
    -- ManufacturingQC.main
    PaperReview.main
    State.PaperForget.main
    State.PaperNothing.main
