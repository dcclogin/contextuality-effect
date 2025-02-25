module Cont.InterpRG where

import SyntaxRG ( ExprRG, BoolRG(R, G) )
import Cont.Effect ( M )

interpRG :: ExprRG -> M (Bool, Bool, Bool)
interpRG (a1, a2, a3) = return (f a1, f a2, f a3)
    where
        f :: BoolRG -> Bool
        f R = True
        f G = False