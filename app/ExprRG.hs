module ExprRG where

import SyntaxRG
import State.ObservableRG
import State.ExperimentRG
import System.Random


-- generate a random ExprRG "instruction set" (R/G,R/G,R/G)
genExprRG :: Int -> ExprRG
genExprRG seed = (if b1 then R else G, if b2 then R else G, if b3 then R else G)
    where
        [b1, b2, b3] = take 3 (randomBoolStream seed)

-- generate a stream of random pairs of particles (up to 10000 pairs)
exprs :: [ExprRG]
exprs = map genExprRG [1..10000]

-- random contexts combinations
ctx11 = [l1, r1]
ctx12 = [l1, r2]
ctx13 = [l1, r3]
ctx21 = [l2, r1]
ctx22 = [l2, r2]
ctx23 = [l2, r3]
ctx31 = [l3, r1]
ctx32 = [l3, r2]
ctx33 = [l3, r3]

ctxDict :: Int -> Context
ctxDict n = case n of
    1 -> ctx11
    2 -> ctx12
    3 -> ctx13
    4 -> ctx21
    5 -> ctx22
    6 -> ctx23
    7 -> ctx31
    8 -> ctx32
    9 -> ctx33
    _ -> error "invalid context."


-- generate a random context stream (up to 10000 contexts)
ctx_indices = take 10000 $ randomRs (1, 9 :: Int) $ mkStdGen 23333
ctxs = map ctxDict ctx_indices

getResult :: [ExprRG] -> [Context] -> [Int] -> [[Bool]]
getResult [] _ _ = []
getResult (e:es) (c:cs) (i:is) = runContextS e c i : getResult es cs is
getResult _ _ _ = error "invalid input."

rs = getResult exprs ctxs [10001..20000]

-- get statistics
-- group (1) ctx11, ctx22, ctx33: 1,5,9
--   - TT/FF 100%
--   - TF/FT 0%
-- group (2) the rest: 2,3,4,6,7,8
--   - TT/FF ??%
--   - TF/FT ??%

group1 = [1, 5, 9]
group2 = [2, 3, 4, 6, 7, 8]

getStats :: [Int] -> [[Bool]] -> (Int, Int, Int, Int)
getStats [] [] = (0, 0, 0, 0)
getStats (i:is) (r:rs) = if i `elem` group1 then
    let (s1, d1, s2, d2) = getStats is rs in
        if r == [True, True] || r == [False, False] then
            (s1 + 1, d1, s2, d2)
        else
            (s1, d1 + 1, s2, d2)
    else
        let (s1, d1, s2, d2) = getStats is rs in
            if r == [True, True] || r == [False, False] then
                (s1, d1, s2 + 1, d2)
            else
                (s1, d1, s2, d2 + 1)
getStats _ _ = error "invalid input."



printRun :: IO ()
printRun = do
    putStrLn $ show (getStats ctx_indices rs)