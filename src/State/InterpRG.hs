module State.InterpRG where

import SyntaxRG ( ExprRG, BoolRG(R, G) )
import State.Effect ( M, M2 )


-- there is no need for an interpreter for the "instruction set" model
-- simply turn R/G into True/False for the sake of simplicity

interpRG :: ExprRG -> M2 (Bool, Bool, Bool)
interpRG (a1, a2, a3) = return (f a1, f a2, f a3)
    where
        f :: BoolRG -> Bool
        f R = True
        f G = False
