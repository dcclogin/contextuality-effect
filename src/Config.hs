module Config where

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

-- quantum paper
data Paper = Paper { margins   :: Maybe Decision
                   , fontSize  :: Maybe Decision
                   , numPages  :: Maybe Decision
                   } deriving (Eq, Show)


type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)

-- generate a random decision
randomDecision :: IO Decision
randomDecision = do
  b <- randomIO
  return $ if b then Pass else Fail

-- generate a random paper with random decisions for each property
randomPaper :: IO Paper
randomPaper = Paper <$> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision)


-- randomly choose a formatting property
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)


-- collect statistics
runReviewerAgreement :: Int -> IO ReviewerAgreement -> IO (Map.Map ReviewerAgreement Int)
runReviewerAgreement n runTrial = do
  results <- replicateM n runTrial
  return $ Map.fromListWith (+) [ (r, 1) | r <- results ]


-- print statistics
printStats :: String -> Int -> IO ReviewerAgreement -> IO ()
printStats modelName numOfTrial runTrial = do
  args <- getArgs
  let n = maybe numOfTrial read (listToMaybe args)

  counts <- runReviewerAgreement n runTrial

  let total same = sum [ c | ((s, _), c) <- Map.toList counts, s == same ]
      getPct same agree =
        let count = Map.findWithDefault 0 (same, agree) counts
        in if total same == 0 then 0 else fromIntegral count * 100 / fromIntegral (total same) :: Double

  putStrLn $ "PaperReview " ++ modelName ++ ": Ran " ++ show n ++ " trials.\n"
  putStrLn "Category                          Percent"
  printEntry "SameProperty & SameDecision" (getPct True  True)
  printEntry "SameProperty & DiffDecision" (getPct True  False)
  printEntry "DiffProperty & SameDecision" (getPct False True)
  printEntry "DiffProperty & DiffDecision" (getPct False False)
  putStrLn ""

  where
    printEntry label pct = putStrLn $ padRight 35 label ++ showFF pct ++ " %"
    padRight n s = s ++ replicate (n - length s) ' '
    showFF = printf "%.2f"
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x


