module Main (Main.main) where


import Pure.PaperSomething
import Pure.SuperDeterminism
import Pure.ContextDependent
import State.PaperForget
import State.PaperNothing
import State.PaperOthing
import Cont.PaperNothing
import Cont.PaperOthing
import Concur.PaperForget
import Concur.PaperNothing

main :: IO ()
main = do
    Pure.PaperSomething.main
    Pure.SuperDeterminism.main
    Pure.ContextDependent.main
    State.PaperForget.main
    State.PaperNothing.main
    State.PaperOthing.main
    Cont.PaperNothing.main
    Cont.PaperOthing.main
    Concur.PaperForget.main
    Concur.PaperNothing.main

