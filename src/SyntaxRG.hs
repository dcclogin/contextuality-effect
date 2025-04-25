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

type Record1O = (Config, Outcome)
type Record2O = (Config, (Outcome, Outcome))

type Pred1O = Record1O -> Bool
type Pred2O = Record2O -> Bool

-- predicates of Record2Os
spso, spdo, dpso, dpdo :: Pred2O
spso ((pos1, pos2), (o1, o2)) = (pos1 == pos2 && o1 == o2)
spdo ((pos1, pos2), (o1, o2)) = (pos1 == pos2 && o1 /= o2)
dpso ((pos1, pos2), (o1, o2)) = (pos1 /= pos2 && o1 == o2)
dpdo ((pos1, pos2), (o1, o2)) = (pos1 /= pos2 && o1 /= o2)


-- traverse 4 times!!
allStats :: [Record2O] -> (Int, Int, Int, Int)
allStats rs = 
    let spsoCount = length (filter spso rs)
        spdoCount = length (filter spdo rs)
        dpsoCount = length (filter dpso rs)
        dpdoCount = length (filter dpdo rs)
    in (spsoCount, spdoCount, dpsoCount, dpdoCount)


