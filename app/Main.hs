module Main (main) where

import ExprRGConcur
import ExprRGState

main :: IO ()
main = do
    ExprRGState.printRun 10000 24444
    ExprRGConcur.printRun 10000 23333