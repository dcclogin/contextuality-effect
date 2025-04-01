module Cont.InterpRG where

import SyntaxRG ( ExprRG, BoolRG(R, G) )

interpRG :: ExprRG -> (Bool, Bool, Bool)
interpRG (a1, a2, a3) = (f a1, f a2, f a3)
    where
        f :: BoolRG -> Bool
        f R = True
        f G = False