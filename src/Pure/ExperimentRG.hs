module Pure.ExperimentRG where

import SyntaxRG
import RandomUtils
import System.Random (StdGen)
import Pure.ObservableRG (l1, l2, l3, r1, r2, r3)

type System = ExprRG
type Observable = System -> Outcome
type Context = (Observable, Observable)
type Experiment = System -> Context -> (Outcome, Outcome)

-- outcome of a single observable
exp1 :: System -> Observable -> Outcome
exp1 qState f = f qState

-- joint outcomes of 2 observables
exp2 :: System -> Context -> (Outcome, Outcome)
exp2 qState (f1, f2) = (f1 qState, f2 qState)

run1 :: System -> Observable -> Outcome
run1 = exp1

run2 :: System -> Context -> (Outcome, Outcome)
run2 = exp2

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

exec :: System -> Config -> Record2O
exec qState config =
    let ctx = configToContext config
        res = run2 qState ctx
    in (config, res)

execl :: [System] -> [Config] -> [Record2O]
execl es cs = zipWith exec es cs

runExperiment :: Int -> StdGen -> IO ()
runExperiment n gen = do
    -- "eager generation" of expressions and configurations
    let (es, g1) = genExprRGs n gen
        (cs, g2) = genConfigs n g1
        (s1, d1, s2, d2) = allStats (execl es cs) in do
            putStrLn "\nMeasurement results (Pure)"
            putStrLn $ "{XX (pos1 == pos2, 3)} {RR,GG}: " ++ show s1
            putStrLn $ "{XX (pos1 == pos2, 3)} {RG,GR}: " ++ show d1
            putStrLn $ "{XY (pos1 != pos2, 6)} {RR,GG}: " ++ show s2
            putStrLn $ "{XY (pos1 != pos2, 6)} {RG,GR}: " ++ show d2
            putStrLn $ "Total number of records: " ++ show n