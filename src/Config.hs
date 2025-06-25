{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
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

data Direction = L | R
  deriving (Eq, Ord, Show)

-- Each property passes or fails a rule
data Decision = Fail | Pass
  deriving (Eq, Ord, Show)

-- classical paper
data PaperC = PaperC {
    marginsC  :: Decision
  , fontSizeC :: Decision
  , numPagesC :: Decision
} deriving (Eq, Show)

-- quantum paper
data Paper = Paper { 
    margins  :: Maybe Decision
  , fontSize :: Maybe Decision
  , numPages :: Maybe Decision
} deriving (Eq, Show)

{--
-- a Copy is what a Paper appears/discloses its interface to reviewers
-- a.k.a. what is "observable" of a Paper
-- pure version: an object's identity is determined fully by a collection of predicates
-- related: Leibniz's Law => Observational Equivalence
--}

-- the appearance of Paper; interface for the reviewers
type Copy m = Property -> m Decision
-- observables as effectful boolean variables
type Observable m = m Decision
-- reviewer's choice as an input
data Reviewer = Reviewer { choice :: IO Property }

-- distribute one copy (particle)
distribute1 :: (Monad m) => Copy m -> IO (Copy m)
distribute1 particle = return particle

-- distribute two copies (particles) forming a context
distribute2 :: (Monad m) => Copy m -> Copy m -> IO (Context (Copy m))
distribute2 particle1 particle2 = return $ Context (particle1, particle2)


-- [TODO]: "extensible"
data (Monad m) => Model s m = Model {
    source      :: IO s
  , copies      :: IO (Context (Copy m))
  , reviewers   :: Context Reviewer
  , runSolo     :: Copy m -> Context Property 
                -> IO (Context Decision)
  , runSoloS    :: s -> Copy m -> Context Property 
                -> IO (Context Decision)
  , runContext  :: Context (Copy m) -> Context Property 
                -> IO (Context Decision)
  , runContextS :: s -> Context (Copy m) -> Context Property 
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


notD :: Bool -> Decision
notD False = Fail
notD True  = Pass

notM :: (Monad m) => Decision -> m Decision
notM Fail = return Pass
notM Pass = return Fail

{--
type Mod = Outcome -> Outcome
type ModList = [Mod]

-- the order of Mod matters
applyMod :: Outcome -> ModList -> Outcome
applyMod o [] = o
applyMod o (m:ms) = applyMod (m o) ms
--}


entangle :: (Monad m) => Context (m Decision) -> m (Context Decision)
entangle = sequence


-- pov: reviewers
executeModel :: (Monad m) => Model s m -> IO (Context Outcome)
executeModel model = do
  cs <- copies model
  ps <- sequence $ choice <$> reviewers model
  ds <- (runContext model) cs ps
  return $ Outcome <$> ps <*> ds

-- pov: "objective"
executeModelS :: (Monad m) => Model s m -> IO (Context Outcome)
executeModelS model = do
  hvar <- source model
  cs <- copies model
  ps <- sequence $ choice <$> reviewers model
  ds <- (runContextS model) hvar cs ps
  return $ Outcome <$> ps <*> ds


type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)


getAgreement :: IO (Context Outcome) -> IO ReviewerAgreement
getAgreement outcomes = do
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
printStats nameOfModel numOfTrial runTrial = do
  args <- getArgs
  let n = maybe numOfTrial read (listToMaybe args)

  counts <- runReviewerAgreement n runTrial

  let total same = sum [ c | ((s, _), c) <- Map.toList counts, s == same ]
      getPct same agree =
        let count = Map.findWithDefault 0 (same, agree) counts
        in if total same == 0 then 0 else fromIntegral count * 100 / fromIntegral (total same) :: Double

  putStrLn $ "PaperReview " ++ nameOfModel ++ ": Ran " ++ show n ++ " trials.\n"
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


