module Main (Main.main) where


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

main :: IO ()
main = do
    Pure.PaperSomething.main
    Pure.SuperDeterminism.main
    Pure.ContextDependent.main
    State.PaperCheating.main
    State.PaperForget.main
    State.PaperNothing.main
    State.PaperOthing.main
    Cont.PaperNothing.main
    Cont.PaperOthing.main
    Concur.PaperForget.main
    Concur.PaperNothing.main
    Concur.PaperOthing.main

