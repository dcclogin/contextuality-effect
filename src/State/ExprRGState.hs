module State.ExprRGState where

import SyntaxRG
import RandomUtils
import State.ObservableRG
import State.ExperimentRG
import System.Random
import Control.Monad.State.Lazy ( evalState )


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

-- execute with the record of the configuration/context
exec :: StdGen -> Qsystem -> Config -> ((Config, (Outcome, Outcome)), StdGen)
exec gen qState config = 
    let ctx = configToContext config
        (res, g) = run2 gen qState ctx
    in ((config, res), g)

-- multiple executions with records
execl :: StdGen -> [Qsystem] -> [Config] -> ([(Config, (Outcome, Outcome))], StdGen)
execl gen [] _ = ([], gen)
execl gen (e:es) (c:cs) =
    let (r, g1) = exec gen e c
        (rs, g2) = execl g1 es cs
    in (r:rs, g2)

{- -- execute with one customized question (regarding original outcomes)
execq :: Query -> Qsystem -> Config -> (Config, Outcome)
execq query qState config =
    let ctx = configToContext config
        m = exp2 qState ctx >>= query
        res = evalState m (Nothing, S1, gen)
    in (config, res)

-- multiple executions with records
execql :: Query -> [Qsystem] -> [Config] -> [(Config, Outcome)]
execql query es cs = zipWith (execq query) es cs -}

-- predicates of experiment records
type Pred1O = (Config, Outcome) -> Bool
type Pred2O = (Config, (Outcome, Outcome)) -> Bool

-- [s]ame [p]osition, [s]ame [o]utcome
-- [s]ame [p]osition, [d]ifferent [o]utcome
-- [d]ifferent [p]osition, [s]ame [o]utcome
-- [d]ifferent [p]osition, [d]ifferent [o]utcome
spso, spdo, dpso, dpdo :: Pred2O
spso = \((pos1, pos2), (o1, o2)) -> (pos1 == pos2 && o1 == o2)
spdo = \((pos1, pos2), (o1, o2)) -> (pos1 == pos2 && o1 /= o2)
dpso = \((pos1, pos2), (o1, o2)) -> (pos1 /= pos2 && o1 == o2)
dpdo = \((pos1, pos2), (o1, o2)) -> (pos1 /= pos2 && o1 /= o2)

-- traverse the list of records 4 times!!
allStats :: [(Config, (Outcome, Outcome))] -> (Int, Int, Int, Int)
allStats rs =
    let n1 = length $ filter spso rs
        n2 = length $ filter spdo rs
        n3 = length $ filter dpso rs
        n4 = length $ filter dpdo rs
    in (n1, n2, n3, n4)

runExperiment :: Int -> StdGen -> IO ()
runExperiment n gen = do
    -- "eager generation"
    let (es, g1) = genExprRGs n gen
        (cs, g2) = genConfigs n g1
        (s1, d1, s2, d2) = allStats (fst (execl g2 es cs)) in do
            putStrLn "\nMeasurement results (State)"
            putStrLn $ "{XX (pos1 == pos2, 3)} {RR,GG}: " ++ show s1
            putStrLn $ "{XX (pos1 == pos2, 3)} {RG,GR}: " ++ show d1
            putStrLn $ "{XY (pos1 != pos2, 6)} {RR,GG}: " ++ show s2
            putStrLn $ "{XY (pos1 != pos2, 6)} {RG,GR}: " ++ show d2
            putStrLn $ "Total number of records: " ++ show n