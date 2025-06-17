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
data Paper = Paper { 
    margins   :: Maybe Decision
  , fontSize  :: Maybe Decision
  , numPages  :: Maybe Decision
} deriving (Eq, Show)


data Reviewer = Reviewer { 
  choice :: IO Property
}

data Trial s c = Trial {
    source    :: IO s
  , copies    :: IO (c, c)
  , reviewers :: (Reviewer, Reviewer)
}

data Outcome = Outcome {
    property :: Property
  , decision :: Decision
} deriving (Eq, Show)

type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)


getAgreement :: IO (Outcome, Outcome) -> IO ReviewerAgreement
getAgreement outcomes = do
  (o1, o2) <- outcomes
  let sameProperty = (property o1 == property o2)
      sameDecision = (decision o1 == decision o2)
  return (sameProperty, sameDecision)


-- blueprint for Nothing model (for all papers rendered on-the-fly)
-- <The Paper> has no intrinsic properties: 
-- there is only one indistinguishable paper which appears differently 
thePaper :: Paper
thePaper = Paper Nothing Nothing Nothing


reg :: Bool -> Decision
reg False = Fail
reg True  = Pass


-- generate a random decision
randomDecision :: IO Decision
randomDecision = do b <- randomIO; return $ reg b


-- generate a random paper with random decisions for each property
randomPaper :: IO Paper
randomPaper = Paper <$> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision)


randomDecision3 :: IO (Decision, Decision, Decision)
randomDecision3 = do
  b1 <- randomIO
  b2 <- randomIO
  b3 <- randomIO
  if (b1 == b2) && (b2 == b3)
    then randomDecision3
    else return (reg b1, reg b2, reg b3)


-- generate a random paper (with 0 probability for PPP and FFF)
randomPaper0 :: IO Paper
randomPaper0 = do
  (dec1, dec2, dec3) <- randomDecision3
  return $ Paper (Just dec1) (Just dec2) (Just dec3)


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


