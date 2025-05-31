-- A contextuality experiment using a paper
-- with three numerical formatting properties: margins, font size, and page count

module PaperReview where

import System.Random
import System.Environment (getArgs)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Text.Printf

-- The three formatting properties
data Property = Margins | FontSize | NumPages
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Each property passes or fails a rule
data Decision = Fail | Pass
  deriving (Eq, Ord, Show)

-- A paper has three numerical formatting properties
data Paper = Paper { margins   :: Double  -- in inches
                   , fontSize  :: Double  -- in points
                   , numPages  :: Int     -- page count
                   } deriving (Eq, Show)

type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)

-- Randomly generate a margin in range [0.5, 1.5]
randomMargin :: IO Double
randomMargin = randomRIO (0.5, 1.5)

-- Randomly generate a font size in range [10.0, 14.0]
randomFontSize :: IO Double
randomFontSize = randomRIO (10.0, 14.0)

-- Randomly generate a page count in range [10, 30]
randomPageCount :: IO Int
randomPageCount = randomRIO (10, 30)

-- Generate a random paper
randomPaper :: IO Paper
randomPaper = Paper <$> randomMargin <*> randomFontSize <*> randomPageCount

-- Randomly choose a formatting property
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)

-- Source gives the same paper to both reviewers
source :: IO (Paper, Paper)
source = do paper <- randomPaper; return (paper, paper)

-- Inspection logic based on passing thresholds
inspect :: Paper -> Property -> Decision
inspect paper prop = case prop of
  Margins   -> if abs (margins paper - 1.0) < 0.01 then Pass else Fail
  FontSize  -> if abs (fontSize paper - 12.0) < 0.01 then Pass else Fail
  NumPages  -> if numPages paper < 20 then Pass else Fail

-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  (paper1, paper2) <- source
  let sameProperty = p1 == p2
      sameDecision = inspect paper1 p1 == inspect paper2 p2
  return (sameProperty, sameDecision)

-- Collect statistics
runReviewerAgreement :: Int -> IO (Map.Map ReviewerAgreement Int)
runReviewerAgreement n = do
  results <- replicateM n runTrial
  return $ Map.fromListWith (+) [ (r, 1) | r <- results ]

-- Main program
main :: IO ()
main = do
  args <- getArgs
  let n = maybe 10000 read (listToMaybe args)

  counts <- runReviewerAgreement n

  let total same = sum [ c | ((s, _), c) <- Map.toList counts, s == same ]
      getPct same agree =
        let count = Map.findWithDefault 0 (same, agree) counts
        in if total same == 0 then 0 else fromIntegral count * 100 / fromIntegral (total same) :: Double

  putStrLn $ "PaperReview: Ran " ++ show n ++ " trials.\n"
  putStrLn "Category                          Percent"
  printEntry "SameProperty & SameDecision" (getPct True  True)
  printEntry "SameProperty & DiffDecision" (getPct True  False)
  printEntry "DiffProperty & SameDecision" (getPct False True)
  printEntry "DiffProperty & DiffDecision" (getPct False False)

  where
    printEntry label pct = putStrLn $ padRight 35 label ++ showFF pct ++ " %"
    padRight n s = s ++ replicate (n - length s) ' '
    showFF = printf "%.2f"
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x