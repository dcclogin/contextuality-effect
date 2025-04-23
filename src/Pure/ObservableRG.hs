module Pure.ObservableRG where

import SyntaxRG

l1, l2, l3 :: ExprRG -> Outcome
l1 (c1, _, _) = (c1 == R)
l2 (_, c2, _) = (c2 == R)
l3 (_, _, c3) = (c3 == R)

r1, r2, r3 :: ExprRG -> Outcome
r1 = l1
r2 = l2
r3 = l3