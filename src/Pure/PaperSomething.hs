module Pure.PaperSomething where

import Config
import Control.Monad.Identity


-- Source gives the same paper to both reviewers
source :: IO (Paper, Paper)
source = do paper <- randomPaper; return (paper, paper)


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


-- Inspection
inspect :: Paper -> Property -> Decision
inspect paper prop = case getDecision paper prop of
  Just dd -> dd
  Nothing -> error "internal bug."


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  (paper1, paper2) <- source
  let d1 = inspect paper1 p1
      d2 = inspect paper2 p2
      sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)


main :: IO ()
main = do
  printStats "(Pure)" 10000 runTrial
