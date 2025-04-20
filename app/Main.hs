module Main (main) where

import ExprRGConcur
import ExprRGCont
import ExprRGState

main :: IO ()
main = do
    ExprRGState.printRun 10000 77777
    ExprRGConcur.printRun 10000 88888
    ExprRGCont.printRun 10000 99999