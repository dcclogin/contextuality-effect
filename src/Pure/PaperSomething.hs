module Pure.PaperSomething where

import Config
import Control.Monad.Identity


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


-- a Copy is what a Paper appears/discloses its interface to reviewers
-- a.k.a. what is "observable" of a Paper
-- intuition: an object's identity is determined fully by a collection of predicates
type Copy = Property -> Decision


-- paper is equivalent to a classical hidden variable
cp :: Paper -> Copy
cp paper = \prop -> case getDecision paper prop of
  Just dec -> dec
  Nothing  -> error "internal bug." 


makeCopy :: Paper -> IO (Copy, Copy)
makeCopy paper = return (cp paper, cp paper)


-- Inspection
inspect :: Copy -> Property -> IO Decision
inspect copy prop = return $ copy prop


inspect2 :: (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect2 (copy1, copy2) (prop1, prop2) = do
  dec1 <- inspect copy1 prop1
  dec2 <- inspect copy2 prop2
  return (dec1, dec2)


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  paper <- randomPaper
  (copy1, copy2) <- makeCopy paper
  (d1, d2) <- inspect2 (copy1, copy2) (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)


main :: IO ()
main = do
  printStats "(Pure)" 10000 runTrial
