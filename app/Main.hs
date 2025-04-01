module Main (main) where

import ExprRGConcur
import ExprRGCont
import ExprRGState
import Prototype01
import Prototype02

main :: IO ()
main = do
    ExprRGState.printRun 10000 24444
    ExprRGConcur.printRun 10000 25555
    ExprRGCont.printRun 10000 26789