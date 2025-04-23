module State.InterpRG where

import SyntaxRG ( ExprRG, RG(R, G) )
import State.Effect ( M )


-- there is no need for an interpreter for the "instruction set" model
-- interpret R/G as True/False for the sake of simplicity

-- "read-only"
interpRG :: ExprRG -> M (Bool, Bool, Bool)
interpRG (a1, a2, a3) = return (f a1, f a2, f a3)
    where
        f :: RG -> Bool
        f R = True
        f G = False
