module Concur.ObservableRG where

import SyntaxRG ( ExprRG )
import Concur.Effect ( ChannelM, ChannelT )
import Concur.InterpRG ( interpRG )
import Concur.ExperimentRG ( Outcome )
import Control.Concurrent.STM ( STM, writeTVar, readTVar )


-- 6 observables:
-- 3 for the left receiver, 3 for the right receiver
l1, l2, l3 :: ExprRG -> ChannelT -> STM Outcome
l1 e c = do
    (b1, _, _) <- interpRG e c
    (b, n) <- readTVar c
    if n == 0 then do
        writeTVar c (b1, 1)
        return b1
    else if n == 1 then
        return b1
    else if b1 == b then
        return (not b1)
    else
        return b1
l2 e c = do
    (_, b2, _) <- interpRG e c
    (b, n) <- readTVar c
    if n == 0 then do
        writeTVar c (b2, 2)
        return b2
    else if n == 2 then
        return b2
    else if b2 == b then
        return (not b2)
    else
        return b2
l3 e c = do
    (_, _, b3) <- interpRG e c
    (b, n) <- readTVar c
    if n == 0 then do
        writeTVar c (b3, 3)
        return b3
    else if n == 3 then
        return b3
    else if b3 == b then
        return (not b3)
    else
        return b3

r1, r2, r3 :: ExprRG -> ChannelT -> STM Outcome
r1 = l1
r2 = l2
r3 = l3

