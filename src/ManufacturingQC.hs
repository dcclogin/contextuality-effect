-- A contextuality experiment using a manufactured mechanical part
-- with three intrinsic properties (diameter, weight, hardness)

module ManufacturingQC where

import System.Random
import System.Environment (getArgs)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Text.Printf

-- Three intrinsic, measurable properties
-- Each can pass (✓) or fail (✗)
data Property = Diameter | Weight | Hardness
  deriving (Eq, Ord, Show, Enum, Bounded)

data Decision = Fail | Pass
  deriving (Eq, Ord, Show)

-- The part has three binary intrinsic properties
-- Assumed to be fixed, classical values
data Part = Part { diameter :: Decision
                 , weight   :: Decision
                 , hardness :: Decision }
  deriving (Eq, Show)

type InspectorAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)

-- Randomly generate a Pass/Fail decision
randomDecision :: IO Decision
randomDecision = do b <- randomIO; return $ if b then Fail else Pass

-- Randomly generate a part with three intrinsic properties
randomPart :: IO Part
randomPart = Part <$> randomDecision <*> randomDecision <*> randomDecision

-- Randomly choose one of the three properties
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)

-- The source always gives the same part to both inspectors
source :: IO (Part, Part)
source = do part <- randomPart; return (part, part)

-- Inspect a particular property of a part
inspect :: Part -> Property -> Decision
inspect part prop = case prop of
  Diameter -> diameter part
  Weight   -> weight part
  Hardness -> hardness part

-- Run one trial of the experiment
runTrial :: IO InspectorAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  (part1, part2) <- source
  let sameProperty = p1 == p2
      sameDecision = inspect part1 p1 == inspect part2 p2
  return (sameProperty, sameDecision)

-- Run n trials and collect statistics
runInspectorAgreement :: Int -> IO (Map.Map InspectorAgreement Int)
runInspectorAgreement n = do
  results <- replicateM n runTrial
  return $ Map.fromListWith (+) [ (r, 1) | r <- results ]

-- Main program with summary output
main :: IO ()
main = do
  args <- getArgs
  let n = maybe 10000 read (listToMaybe args)

  counts <- runInspectorAgreement n

  let total same = sum [ c | ((s, _), c) <- Map.toList counts, s == same ]
      getPct same color = 
        let count = Map.findWithDefault 0 (same, color) counts
        in if total same == 0 then 0 else fromIntegral count * 100 / fromIntegral (total same) :: Double

  putStrLn $ "Ran " ++ show n ++ " trials.\n"
  putStrLn "Category                     Percent"
  printEntry "SameProperty & SameDecision" (getPct True  True)
  printEntry "SameProperty & DiffDecision" (getPct True  False)
  printEntry "DiffProperty & SameDecision" (getPct False True)
  printEntry "DiffProperty & DiffDecision" (getPct False False)

  where
    printEntry label pct = putStrLn $ padRight 30 label ++ showFF pct ++ " %"
    padRight n s = s ++ replicate (n - length s) ' '
    showFF = printf "%.2f"
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x