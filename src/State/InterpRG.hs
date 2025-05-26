module State.InterpRG where

import SyntaxRG ( RG(R, G), RGU, Outcome )
import System.Random
import RandomUtils
import Control.Monad.State.Lazy


data PosL = L1 | L2 | L3
    deriving (Show, Eq)
data PosR = R1 | R2 | R3
    deriving (Show, Eq)

-- alias: <simple> observable
type Observable = Either PosL PosR

-- alias: <compound> measurement, boolean algebra
data Question = FF | TT
    | LL PosL | RR PosR
    | Not Question 
    | And Question Question 
    | Or Question Question
    deriving (Show, Eq)


-- local and nonlocal hidden variables
type HiddenVar = (RG, RG, RG)
type HiddenVarU = (RGU, RGU, RGU)
-- state monad with hidden variables and a random generator
type MU = State (HiddenVarU, StdGen)


-- collection of predicates
type SystemCs = Observable -> Outcome
type SystemC = Question -> Outcome
-- effectful <quantum> version
type SystemQs = Observable -> MU Outcome
type SystemQ = Question -> MU Outcome



-- intuition: whether two questions can be asked together
isCommeasurable :: Question -> Question -> Bool
isCommeasurable q1 q2 =
    case (q1, q2) of
        (FF, _) -> True
        (_, FF) -> True
        (TT, _) -> True
        (_, TT) -> True
        (q1, q2) | isLeft q1 && isRight q2 -> True
        (q1, q2) | isRight q1 && isLeft q2 -> True
        (Not q1, q2) -> isCommeasurable q1 q2
        (q1, Not q2) -> isCommeasurable q1 q2
        (Not q1, Not q2) -> isCommeasurable q1 q2
        -- [TODO]
        -- (And q1 q2, _) -> isCommeasurable q1 q2
        -- (_, And q1 q2) -> isCommeasurable q1 q2
        _ -> False
    where
        isLeft, isRight :: Question -> Bool
        isLeft (LL _) = True
        isLeft _ = False
        isRight (RR _) = True
        isRight _ = False


-- local hidden variable model <instruction set>
-- <determinism>
-- <locality>
-- <non-contextuality>
csyss :: (HiddenVar, HiddenVar) -> SystemCs
csyss ((a1, a2, a3), (b1, b2, b3)) = \obs ->
    case obs of
        Left L1 -> (a1 == R)
        Left L2 -> (a2 == R)
        Left L3 -> (a3 == R)
        Right R1 -> (b1 == R)
        Right R2 -> (b2 == R)
        Right R3 -> (b3 == R)


-- [TODO] qsyss
-- nonlocal hidden variable
qsyss :: SystemQs
qsyss obs = do
    ((x1, x2, x3), gen) <- get
    case obs of
        Left L1 -> return True
        Left L2 -> return True
        Left L3 -> return True
        Right R1 -> return True
        Right R2 -> return True
        Right R3 -> return True



-- [TODO] csys
-- csys is a partial Boolean Algebra with
-- 6 variables {L1, L2, L3, R1, R2, R3} and a binary relation ⊙ <commesurable>


-- [TODO] qsys
-- qsys is a partial Boolean Algebra with
-- 6 variables {L1, L2, L3, R1, R2, R3} and a binary relation ⊙ <commesurable>