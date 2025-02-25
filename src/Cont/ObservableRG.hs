module Cont.ObservableRG where

import SyntaxRG ( ExprRG )
import Cont.Effect ( M )
import Cont.ExperimentRG ( Outcome )
import Control.Monad.Cont


-- 6 observables:
-- 3 for the left receiver, 3 for the right receiver
l1, l2, l3 :: ExprRG -> (Outcome -> M Outcome) -> M Outcome
l1 (b1, _, _) k = callCC $ \exit -> do
    callCC $ \resumeL -> do
        k b1 >>= resumeL
    exit b1
l2 (_, b2, _) k = callCC $ \exit -> do
    callCC $ \resumeL -> do
        k b2 >>= resumeL
    exit b2
l3 (_, _, b3) k = callCC $ \exit -> do
    callCC $ \resumeL -> do
        k b3 >>= resumeL
    exit b3