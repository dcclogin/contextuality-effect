{-# LANGUAGE UnicodeSyntax #-}
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

csyssL :: HiddenVar -> (PosL -> Outcome)
csyssL (a1, a2, a3) = \l ->
    case l of
        L1 -> (a1 == R)
        L2 -> (a2 == R)
        L3 -> (a3 == R)
csyssR :: HiddenVar -> (PosR -> Outcome)
csyssR (b1, b2, b3) = \r ->
    case r of
        R1 -> (b1 == R)
        R2 -> (b2 == R)
        R3 -> (b3 == R)

(⊠) :: (HiddenVar -> (a -> Outcome)) 
    -> (HiddenVar -> (b -> Outcome)) 
    -> (HiddenVar, HiddenVar) -> (Either a b -> Outcome)
f ⊠ g = \(hvarL, hvarR) -> \obs ->
    case obs of
        Left l -> f hvarL l
        Right r -> g hvarR r

-- <tensor product> of csyssL and csyssR
csyss :: (HiddenVar, HiddenVar) -> SystemCs
csyss = csyssL ⊠ csyssR



-- [TODO] qsyss
-- nonlocal hidden variable

roll :: StdGen -> MU (RG, StdGen)
roll gen = return (genColor gen)

qsyssL :: PosL -> MU Outcome
qsyssL L1 = do
    (hvar, gen) <- get
    case hvar of
        (Just v1, _, _) -> return (v1 == R)
        (Nothing, x2, x3) -> do
            (v1, g) <- roll gen -- roll an answer according to the protocol
            case (x2, x3) of
                (Nothing, Nothing) -> do
                    put ((Just v1, x2, x3), g)
                    return (v1 == R)
                (Just v2, Nothing) | v2 /= v1 -> do
                    put ((Just v1, x2, x3), g)
                    return (v1 == R)
                (Just v2, Nothing) | v2 == v1 -> do
                    (v1', g') <- roll g -- reroll an answer
                    put ((Just v1', x2, x3), g')
                    return (v1' == R)
                (Nothing, Just v3) | v3 /= v1 -> do
                    put ((Just v1, x2, x3), g)
                    return (v1 == R)
                (Nothing, Just v3) | v3 == v1 -> do
                    (v1', g') <- roll g -- reroll an answer
                    put ((Just v1', x2, x3), g')
                    return (v1' == R)
                _ -> error "internal error: impossible state in qsyssL"
qsyssL L2 = do
    (hvar, gen) <- get
    case hvar of
        (_, Just v2, _) -> return (v2 == R)
        (x1, Nothing, x3) -> do
            (v2, g) <- roll gen -- roll an answer according to the protocol
            case (x1, x3) of
                (Nothing, Nothing) -> do
                    put ((x1, Just v2, x3), g)
                    return (v2 == R)
                (Just v1, Nothing) | v1 /= v2 -> do
                    put ((x1, Just v2, x3), g)
                    return (v2 == R)
                (Just v1, Nothing) | v1 == v2 -> do
                    (v2', g') <- roll g -- reroll an answer
                    put ((x1, Just v2', x3), g')
                    return (v2' == R)
                (Nothing, Just v3) | v3 /= v2 -> do
                    put ((x1, Just v2, x3), g)
                    return (v2 == R)
                (Nothing, Just v3) | v3 == v2 -> do
                    (v2', g') <- roll g -- reroll an answer
                    put ((x1, Just v2', x3), g')
                    return (v2' == R)
                _ -> error "internal error: impossible state in qsyssL"
qsyssL L3 = do
    (hvar, gen) <- get
    case hvar of
        (_, _, Just v3) -> return (v3 == R)
        (x1, x2, Nothing) -> do
            (v3, g) <- roll gen -- roll an answer according to the protocol
            case (x1, x2) of
                (Nothing, Nothing) -> do
                    put ((x1, x2, Just v3), g)
                    return (v3 == R)
                (Just v1, Nothing) | v1 /= v3 -> do
                    put ((x1, x2, Just v3), g)
                    return (v3 == R)
                (Just v1, Nothing) | v1 == v3 -> do
                    (v3', g') <- roll g -- reroll an answer
                    put ((x1, x2, Just v3'), g')
                    return (v3' == R)
                (Nothing, Just v2) | v2 /= v3 -> do
                    put ((x1, x2, Just v3), g)
                    return (v3 == R)
                (Nothing, Just v2) | v2 == v3 -> do
                    (v3', g') <- roll g -- reroll an answer
                    put ((x1, x2, Just v3'), g')
                    return (v3' == R)
                _ -> error "internal error: impossible state in qsyssL"

qsyssR :: PosR -> MU Outcome
qsyssR R1 = qsyssL L1
qsyssR R2 = qsyssL L2
qsyssR R3 = qsyssL L3


(⊗) :: (a -> MU Outcome) 
    -> (b -> MU Outcome) 
    -> (Either a b -> MU Outcome)
f ⊗ g = \obs ->
    case obs of
        Left l -> f l
        Right r -> g r

qsyss :: SystemQs
qsyss = qsyssL ⊗ qsyssR



-- [TODO] csys
-- csys is a partial Boolean Algebra with
-- 6 variables {L1, L2, L3, R1, R2, R3} and a binary relation ⊙ <commesurable>


-- [TODO] qsys
-- qsys is a partial Boolean Algebra with
-- 6 variables {L1, L2, L3, R1, R2, R3} and a binary relation ⊙ <commesurable>