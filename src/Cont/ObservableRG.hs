module Cont.ObservableRG where

import SyntaxRG ( ExprRG )
import Cont.Effect
import Cont.InterpRG
import Cont.ExperimentRG ( Outcome )
import Control.Monad.Cont


-- 6 observables:
-- 3 for the left receiver, 3 for the right receiver

l1, l2, l3 :: ExprRG -> Iterator (Bool, Int) (Bool, Int) Outcome
l1 e = let (b, _, _) = interpRG e in 
    runYield $ do
        (b', n) <- yield (b, 1)
        if b' == b then
            if n == 1 then return b
            else return (not b)
        else return b

l2 e = let (_, b, _) = interpRG e in
    runYield $ do
        (b', n) <- yield (b, 2)
        if b' == b then
            if n == 2 then return b
            else return (not b)
        else return b

l3 e = let (_, _, b) = interpRG e in
    runYield $ do
        (b', n) <- yield (b, 3)
        if b' == b then
            if n == 3 then return b
            else return (not b)
        else return b 


r1 :: ExprRG -> Iterator (Bool, Int) (Bool, Int) Outcome
r1 = l1
r2 = l2
r3 = l3
