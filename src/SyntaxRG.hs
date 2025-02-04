{-# LANGUAGE GADTs #-}

module SyntaxRG where

data BoolRG where
    R :: BoolRG
    G :: BoolRG
    deriving (Show)

-- just 3 bits ("instruction sets")
type ExprRG = (BoolRG, BoolRG, BoolRG)