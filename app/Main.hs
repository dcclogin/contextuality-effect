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
ctx6 = [pureContingent, pureContingent]
ctx7 = [pureContingent, causeNot]

-- two by two (Bell configuration)
suite1 = [ctx1, ctx2, ctx3, ctx4]
suite2 = [ctx5, ctx6, ctx7, ctx5, ctx6, ctx7]

-- is it possible to reproduce PR-box like strong contextuality? 
-- add more state with different "frequency"?

-- one by one
-- suite2 = [[pureContingent], [causeFalse], [hasError], [causeTrue]]

-- outcomes highly depend on order of measurements - causal contextuality
-- how to implement "retro-causality"?


main :: IO ()
main = do
    print (runExperimentS e1 suite2 123)
