module Concur.InterpRG where

import SyntaxRG ( ExprRG, RG(R, G) )
import Concur.Effect ( ChannelT )
import Control.Concurrent.STM ( STM )

interpRG :: ExprRG -> ChannelT -> STM (Bool, Bool, Bool)
interpRG (a1, a2, a3) _ = return (f a1, f a2, f a3)
    where
        f :: RG -> Bool
        f R = True
        f G = False