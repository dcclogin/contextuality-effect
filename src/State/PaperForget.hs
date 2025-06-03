module State.PaperForget where

import System.Random
import System.Environment (getArgs)
import Control.Monad (replicateM)
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Text.Printf

-- The three formatting properties
data Property = Margins | FontSize | NumPages
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Each property passes or fails a rule
data Decision = Fail | Pass
  deriving (Eq, Ord, Show)

-- Quantum paper
data Paper = Paper { margins   :: Maybe Decision
                   , fontSize  :: Maybe Decision
                   , numPages  :: Maybe Decision
                   } deriving (Eq, Show)

type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)


randomDecision :: IO Decision
randomDecision = do
  b <- randomIO
  return $ if b then Pass else Fail

randomPaper :: IO Paper
randomPaper = Paper <$> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision)

-- Randomly choose a formatting property
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)

-- Source gives the same paper to both reviewers
source :: IO (Paper, Paper)
source = do paper <- randomPaper; return (paper, paper)


-- nonlocal hidden variable as state monad
-- type M = State Paper
type M = StateT Paper IO


getDecision :: Property -> M (Maybe Decision)
getDecision prop = do
  paper <- get
  case prop of
    Margins   -> return (margins paper)
    FontSize  -> return (fontSize paper)
    NumPages  -> return (numPages paper)

  
putDecision :: Property -> Maybe Decision -> M ()
putDecision prop d = do
  paper <- get
  let newPaper = case prop of
        Margins   -> paper { margins = d }
        FontSize  -> paper { fontSize = d }
        NumPages  -> paper { numPages = d }
  put newPaper

-- unconditional forget
forgetDecision :: Property -> M ()
forgetDecision prop = putDecision prop Nothing

-- conditional forget
cforgetDecision :: Property -> (Maybe Decision -> Bool) -> M ()
cforgetDecision prop pred = do
  d <- getDecision prop
  if pred d then forgetDecision prop else return ()

-- forgetful-get
getDecisionF :: Property -> M (Maybe Decision)
getDecisionF Margins = do
  m <- getDecision Margins
  cforgetDecision FontSize (== m)
  cforgetDecision NumPages (== m)
  return m
getDecisionF FontSize = do
  m <- getDecision FontSize
  cforgetDecision Margins (== m)
  cforgetDecision NumPages (== m)
  return m
getDecisionF NumPages = do
  m <- getDecision NumPages
  cforgetDecision Margins (== m)
  cforgetDecision FontSize (== m)
  return m
  


-- criteria for Pass/Fail decisions
-- impossible for <Nothing> to appear to the reviewers
{--
judgeMargin :: Double -> Decision
judgeMargin m = if abs (m - 1.0) < 0.25 then Pass else Fail

judgeFontSize :: Double -> Decision
judgeFontSize fs = if abs (fs - 12.0) < 1.0 then Pass else Fail

judgeNumPages :: Int -> Decision
judgeNumPages np = if np < 20 then Pass else Fail
--}

-- [TODO]: come up with a natural way to express forgetting model


-- the main logic for quantum system <appearance>
sys :: Property -> M Decision
sys prop = do
  d <- getDecisionF prop
  case d of
    Nothing -> do
      dd <- liftIO randomDecision
      putDecision prop (Just dd)
      return dd
    Just dd -> return dd


-- bipartite system
(⨷) :: (Property -> M Decision) 
    -> (Property -> M Decision) 
    -> (Property, Property) -> M (Decision, Decision)
(sys1 ⨷ sys2) (prop1, prop2) = do
  d1 <- sys1 prop1
  d2 <- sys2 prop2
  return (d1, d2)



inspect1 :: Paper -> Property -> IO Decision
inspect1 paper prop = evalStateT (sys prop) paper

inspect2 :: Paper -> (Property, Property) -> IO (Decision, Decision)
inspect2 paper (prop1, prop2) =
    let m = (sys ⨷ sys) (prop1, prop2) in 
      evalStateT m paper


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  paper <- randomPaper
  (d1, d2) <- inspect2 paper (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
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