module State.ExprRGState where

import SyntaxRG
import RandomUtils
import State.ObservableRG
import State.ExperimentRG
import System.Random
import Control.Monad.State.Lazy ( evalState )

data Ctx = Ctx11 | Ctx12 | Ctx13 | Ctx21 | Ctx22 | Ctx23 | Ctx31 | Ctx32 | Ctx33
    deriving (Show, Eq, Enum)

ctxCollection :: [Ctx]
ctxCollection = [Ctx11 ..]

configToContext :: Config -> Context
configToContext config = case config of
    (S1, S1) -> (l1, r1)
    (S1, S2) -> (l1, r2)
    (S1, S3) -> (l1, r3)
    (S2, S1) -> (l2, r1)
    (S2, S2) -> (l2, r2)
    (S2, S3) -> (l2, r3)
    (S3, S1) -> (l3, r1)
    (S3, S2) -> (l3, r2)
    (S3, S3) -> (l3, r3)

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


randomListPure :: Int -> [a] -> Int -> [a]
randomListPure n xs seed =
    let indices = take n $ randomRs (0, length xs - 1) (mkStdGen seed)
    in map (xs !!) indices

-- execute with the record of the configuration/context
exec :: Qsystem -> Config -> (Config, (Outcome, Outcome))
exec qState config = 
    let ctx = configToContext config
        res = run2 qState ctx
    in (config, res)

-- multiple executions with records
execl :: [Qsystem] -> [Config] -> [(Config, (Outcome, Outcome))]
execl es cs = zipWith exec es cs

-- execute with one customized question (regarding original outcomes)
execq :: Query -> Qsystem -> Config -> (Config, Outcome)
execq query qState config =
    let ctx = configToContext config
        m = exp2 qState ctx >>= query
        res = evalState m (Nothing, S1)
    in (config, res)

-- multiple executions with records
execql :: Query -> [Qsystem] -> [Config] -> [(Config, Outcome)]
execql query es cs = zipWith (execq query) es cs


getResult :: [Qsystem] -> [Context] -> [(Outcome, Outcome)]
getResult es cs = zipWith run2 es cs

-- get statistics
-- group (1) ctx11, ctx22, ctx33: 1,5,9
--   - TT/FF 100%
--   - TF/FT 0%
-- group (2) the rest: 2,3,4,6,7,8
--   - TT/FF ??%
--   - TF/FT ??%

getStats :: [Ctx] -> [(Outcome, Outcome)] -> (Int, Int, Int, Int)
getStats [] [] = (0, 0, 0, 0)
getStats (c:cs) (r:rs) = 
    let (s1, d1, s2, d2) = getStats cs rs
    in if c == Ctx11 || c == Ctx22 || c == Ctx33
        then if r == (True, True) || r == (False, False)
            then (s1 + 1, d1, s2, d2)
            else (s1, d1 + 1, s2, d2)
        else if r == (True, True) || r == (False, False)
            then (s1, d1, s2 + 1, d2)
            else (s1, d1, s2, d2 + 1)

printRun :: Int -> Int -> IO ()
printRun n seed = do 
    let exprs = (fst (genExprRGs n (mkStdGen 101))) :: [Qsystem]
        ctxs = randomListPure n ctxCollection seed :: [Ctx]
        contexts = map getContext ctxs :: [Context]
        rs = getResult exprs contexts
        (s1, d1, s2, d2) = getStats ctxs rs in do
            putStrLn "\nMeasurement results (State):"
            print $ "RR/GG for Ctx11, Ctx22, Ctx33: " ++ show s1
            print $ "RG/GR for Ctx11, Ctx22, Ctx33: " ++ show d1
            print $ "RR/GG for Ctx12, Ctx13, Ctx21, Ctx23, Ctx31, Ctx32: " ++ show s2
            print $ "RG/GR for Ctx12, Ctx13, Ctx21, Ctx23, Ctx31, Ctx32: " ++ show d2