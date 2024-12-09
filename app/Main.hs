module Main (main) where

import Syntax
import Observable
import Experiment

e1, e2, e3 :: Expr
e1 = NotE (Var "s")
e2 = LetE "x" (AndE (NotE (Const True)) (Const False)) (NotE (Var "x"))
e3 = PairE (PairE (Const True) e2) (Const True)

-- Qsystem independent contextuality...
ctx1 = [causeTrue, pureContingent]
ctx2 = [causeFalse, pureContingent]
ctx3 = [causeTrue, hasError]
ctx4 = [causeFalse, hasError]

-- two by two (Bell configuration)
suite1 = [ctx1,ctx2,ctx3,ctx4]
-- one by one
suite2 = [[pureContingent], [causeFalse], [hasError], [causeTrue]]


printResult :: Qsystem -> [Context] -> Seed -> IO ()
printResult expr [] seed = pure ()
printResult expr (c:cs) seed = do
    print (runExperiment expr c seed)
    printResult expr cs (seed + 1)


main :: IO ()
main = do
    printResult e1 suite1 111
