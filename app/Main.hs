module Main (Main.main) where


import Pure.PaperSomething
import Pure.SuperDeterminism
import Pure.ContextDependent
import State.PaperForget
import State.PaperNothing
import State.PaperOthing
import Cont.PaperNothing
import Cont.PaperOthing

main :: IO ()
main = do
    Pure.PaperSomething.main
    putStrLn ""
    Pure.SuperDeterminism.main
    putStrLn ""
    Pure.ContextDependent.main
    putStrLn ""
    State.PaperForget.main
    putStrLn ""
    State.PaperNothing.main
    putStrLn ""
    State.PaperOthing.main
    putStrLn ""
    Cont.PaperNothing.main
    putStrLn ""
    Cont.PaperOthing.main
    putStrLn ""
