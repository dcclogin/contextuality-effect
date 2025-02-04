module State.ObservableRG where

import SyntaxRG ( ExprRG, BoolRG(R, G) )
import State.Effect ( M )
import State.ExperimentRG ( Outcome )
import State.InterpRG
import Control.Monad.State.Lazy ( MonadState(put, get) )

-- 6 observables:
-- 3 for the left receiver, 3 for the right receiver


-- vanilla "instruction set" model, effectless
l1, l2, l3 :: ExprRG -> M Outcome
l1 (R, _, _) = return R
l1 (G, _, _) = return G

l2 (_, R, _) = return R
l2 (_, G, _) = return G

l3 (_, _, R) = return R
l3 (_, _, G) = return G

r1, r2, r3 :: ExprRG -> M Outcome
r1 (R, _, _) = return R
r1 (G, _, _) = return G

r2 (_, R, _) = return R
r2 (_, G, _) = return G

r3 (_, _, R) = return R
r3 (_, _, G) = return G

-- with a boolean state for hidden communications?