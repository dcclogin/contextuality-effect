module Main (Main.main) where

import MerminCubes
import ManufacturingQC
import Pure.PaperSomething
import State.PaperForget
import State.PaperNothing
import Cont.PaperNothing
import Cont.PaperOthing

main :: IO ()
main = do
    Pure.PaperSomething.main
    putStrLn ""
    State.PaperForget.main
    putStrLn ""
    State.PaperNothing.main
    putStrLn ""
    Cont.PaperNothing.main
    putStrLn ""
    Cont.PaperOthing.main
    putStrLn ""
