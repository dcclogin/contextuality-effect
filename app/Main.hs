module Main (main) where

import State.ExprRGState
import Concur.ExprRGConcur
import Cont.ExprRGCont

main :: IO ()
main = do
    State.ExprRGState.printRun 10000 77776
    Concur.ExprRGConcur.printRun 10000 88888
    Cont.ExprRGCont.printRun 10000 99999