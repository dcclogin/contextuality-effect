module Config where

import Context2
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
  , measure   :: s -> (c, c) 
                   -> (Property, Property) 
                   -> IO (Decision, Decision)
}

-- reviewer's pov, oblivious to source
data Model m = Model {
    copiesOf      :: IO (Context (Property -> m Decision))
  , reviewersOf   :: Context Reviewer
  , runContextual :: (Property -> m Decision)
                  -> Context Property
                  -> IO (Context Decision)
  , runNonlocal   :: Context (Property -> m Decision) 
                  -> Context Property 
                  -> IO (Context Decision)
}

data Outcome = Outcome {
    property :: Property
  , decision :: Decision
} deriving (Eq, Show)


-- blueprint/mesh for Nothing model (for all papers rendered on-the-fly)
-- <The Paper> has no intrinsic properties: 
-- there is only one indistinguishable paper which appears differently 
thePaper :: Paper
thePaper = Paper Nothing Nothing Nothing


type Mod = Outcome -> Outcome
type ModList = [Mod]


-- the order of Mod matters
applyMod :: Outcome -> ModList -> Outcome
applyMod o [] = o
applyMod o (m:ms) = applyMod (m o) ms


type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)


executeTr :: Trial s c -> IO (Outcome, Outcome)
executeTr tr = do
  hvar <- source tr
  (copy1, copy2) <- copies tr
  prop1 <- choice $ fst $ reviewers tr
  prop2 <- choice $ snd $ reviewers tr
  (dec1, dec2) <- (measure tr) hvar (copy1, copy2) (prop1, prop2)
  return (Outcome prop1 dec1, Outcome prop2 dec2)


execNL :: Model m -> IO (Context Outcome)
execNL model = do
  cs <- copiesOf model
  ps <- sequence $ choice <$> reviewersOf model
  ds <- (runNonlocal model) cs ps
  return $ Outcome <$> ps <*> ds


getAgreement :: IO (Outcome, Outcome) -> IO ReviewerAgreement
getAgreement outcomes = do
  (o1, o2) <- outcomes
  let sameProperty = (property o1 == property o2)
      sameDecision = (decision o1 == decision o2)
  return (sameProperty, sameDecision)


getAgreement' :: IO (Context Outcome) -> IO ReviewerAgreement
getAgreement' outcomes = do
  Context (o1, o2) <- outcomes
  let sameProperty = (property o1 == property o2)
      sameDecision = (decision o1 == decision o2)
  return (sameProperty, sameDecision)


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


