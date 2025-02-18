module ExprRGConcur where

import SyntaxRG
import Concur.ObservableRG
import Concur.ExperimentRG
import System.Random (mkStdGen, randomRs)

data Ctx = Ctx11 | Ctx12 | Ctx13 | Ctx21 | Ctx22 | Ctx23 | Ctx31 | Ctx32 | Ctx33
    deriving (Show, Eq, Enum)

getContext :: Ctx -> Context
getContext ctx = case ctx of
    Ctx11 -> (l1, r1)
    Ctx12 -> (l1, r2)
    Ctx13 -> (l1, r3)
    Ctx21 -> (l2, r1)
    Ctx22 -> (l2, r2)
    Ctx23 -> (l2, r3)
    Ctx31 -> (l3, r1)
    Ctx32 -> (l3, r2)
    Ctx33 -> (l3, r3)

ctxCollection :: [Ctx]
ctxCollection = [Ctx11 ..]

randomListPure :: Int -> [a] -> Int -> [a]
randomListPure n xs seed =
    let indices = take n $ randomRs (0, length xs - 1) (mkStdGen seed)
    in map (xs !!) indices


getStats :: [Ctx] -> [(Outcome, Outcome)] -> (Int, Int, Int, Int)
getStats [] _ = (0, 0, 0, 0)
getStats (c:cs) (r:rs) =
    let (s1, d1, s2, d2) = getStats cs rs
        (o1, o2) = r
    in if c == Ctx11 || c == Ctx22 || c == Ctx33
        then if o1 == o2 then (s1 + 1, d1, s2, d2) else (s1, d1 + 1, s2, d2)
        else if o1 == o2 then (s1, d1, s2 + 1, d2) else (s1, d1, s2, d2 + 1)

printRun :: IO ()
printRun = do
    let n = 5000
        exprs = genQStates n :: [Qsystem]
        ctxs = randomListPure n ctxCollection 33333 :: [Ctx]
        contexts = map getContext ctxs :: [Context] in do
        rs <- runContextsT exprs contexts
        let (s1, d1, s2, d2) = getStats ctxs rs in do
            putStrLn "\nMeasurement results (Concurrency):"
            print $ "RR/GG for Ctx11, Ctx22, Ctx33: " ++ show s1
            print $ "RG/GR for Ctx11, Ctx22, Ctx33: " ++ show d1
            print $ "RR/GG for Ctx12, Ctx13, Ctx21, Ctx23, Ctx31, Ctx32: " ++ show s2
            print $ "RG/GR for Ctx12, Ctx13, Ctx21, Ctx23, Ctx31, Ctx32: " ++ show d2