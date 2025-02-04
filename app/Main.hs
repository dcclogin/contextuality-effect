module Main (main) where

import Syntax
import State.Observable
import State.Experiment
import ExprRG
import System.Random

main :: IO ()
main = do
    printRun