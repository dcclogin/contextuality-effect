module Pure.ExperimentRG where

import SyntaxRG
import Pure.ObservableRG (l1, l2, l3, r1, r2, r3)

type Qsystem = ExprRG
type Observable = Qsystem -> Outcome
type Context = (Observable, Observable)
type Experiment = Qsystem -> Context -> (Outcome, Outcome)

-- outcome of a single observable
exp1 :: Qsystem -> Observable -> Outcome
exp1 qState f = f qState
-- joint outcomes of 2 observables
exp2 :: Qsystem -> Context -> (Outcome, Outcome)
exp2 qState (f1, f2) = (f1 qState, f2 qState)

run1 :: Qsystem -> Observable -> Outcome
run1 qState f = exp1 qState f

run2 :: Qsystem -> Context -> (Outcome, Outcome)
run2 qState ctx = exp2 qState ctx