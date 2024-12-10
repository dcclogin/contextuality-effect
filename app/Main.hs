module Main (main) where

import Syntax
import State.Observable
import State.Experiment

e1, e2, e3 :: Expr
e1 = NotE (Var "s")
e2 = LetE "x" (AndE (NotE (Const True)) (Const False)) (NotE (Var "x"))
e3 = PairE (PairE (Const True) e2) (Const True)

-- Qsystem independent contextuality...
ctx1 = [causeTrue, pureContingent]
ctx2 = [causeFalse, pureContingent]
ctx3 = [causeTrue, hasError]
ctx4 = [causeFalse, hasError]

ctx5 = [causeNot, pureContingent]
ctx6 = [causeId, pureContingent]
ctx7 = [causeNot, hasError]
ctx8 = [causeId, hasError]

-- two by two (Bell configuration)
suite1 = [ctx1, ctx2, ctx3, ctx4]
suite2 = [ctx5, ctx6, ctx7, ctx8]
-- one by one
-- suite2 = [[pureContingent], [causeFalse], [hasError], [causeTrue]]


main :: IO ()
main = do
    printResultS e1 suite1 111
