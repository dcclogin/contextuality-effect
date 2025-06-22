module Main (Main.main) where

{--
import Pure.PaperSomething
import Pure.SuperDeterminism
import Pure.ContextDependent
import State.PaperCheating
import State.PaperForget
import State.PaperNothing
import State.PaperOthing
import Cont.PaperNothing
import Cont.PaperOthing
import Concur.PaperForget
import Concur.PaperNothing
import Concur.PaperOthing
--}


import Experiment
import ExperimentS

main :: IO ()
main = do
  Experiment.main
  ExperimentS.main

