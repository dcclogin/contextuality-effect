{-# LANGUAGE GADTs #-}

module SyntaxRG where

-- color (intrinsic property of a particle)
-- also color flashed on a detector
data RG = R | G
    deriving (Show, Eq)

-- allow "undefined"
-- account for "no intrinsic properties"
type RGU = Maybe RG

-- a quantum state can be a stream of RGU (infinite questions)
-- we simply take 3 in Mermin's experiment
type ExprRG = (RG, RG, RG)
type ExprRGU = (RGU, RGU, RGU)

-- yes/no answer to questions
type Outcome = Bool

-- 3 switch positions on a detector
data Position = S1 | S2 | S3
    deriving (Show, Eq)

-- configuration of two detectors (L and R)
-- each configuration corresponds to a measurement context
type Config = (Position, Position)


