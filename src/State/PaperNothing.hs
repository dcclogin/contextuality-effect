module State.PaperNothing where

import Config
import System.Random
import Control.Monad.State.Lazy


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


-- render a random decision for a property
renderDecision :: Property -> M Decision
renderDecision prop = do
  dd <- liftIO randomDecision
  putDecision prop (Just dd)
  return dd

-- stream of random decisions for a property
-- TODO: integrate with Traversable
renderDecisions :: Property -> M [Decision]
renderDecisions prop = sequenceA $ repeat (renderDecision prop)


-- check if any other properties have made a specific decision 
crecallDecision :: Property -> (Maybe Decision -> Bool) -> M Bool
crecallDecision Margins pred = do
  d1 <- getDecision FontSize
  d2 <- getDecision NumPages
  return (pred d1 || pred d2)
crecallDecision FontSize pred = do
  d1 <- getDecision Margins
  d2 <- getDecision NumPages
  return (pred d1 || pred d2)
crecallDecision NumPages pred = do
  d1 <- getDecision Margins
  d2 <- getDecision FontSize
  return (pred d1 || pred d2)


-- decisions are rendered <by need> (TODO: refine the main logic for <nothing>)
sys :: Property -> M Decision
sys prop = do
  d <- getDecision prop
  case d of
    Just dd -> return dd
    Nothing -> do
      dd <- renderDecision prop
      b <- crecallDecision prop (== Just dd)
      if (not b)
        then return dd
        else renderDecision prop
        -- re-render if the same decision is already made for another property


-- bipartite system
(⨷) :: (Property -> M Decision) 
    -> (Property -> M Decision) 
    -> (Property, Property) -> M (Decision, Decision)
(sys1 ⨷ sys2) (prop1, prop2) = do
  d1 <- sys1 prop1
  d2 <- sys2 prop2
  return (d1, d2)


-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


-- Source gives a copy of paper to both reviewers
source :: IO (Copy, Copy)
source = return (sys, sys)


inspect1 :: Copy -> Property -> IO Decision
inspect1 copy prop = evalStateT (copy prop) thePaper


-- alternative: runStateT
inspect2 :: (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect2 (copy1, copy2) (prop1, prop2) =
  let m = (copy1 ⨷ copy2) (prop1, prop2) in 
    evalStateT m thePaper


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  (copy1, copy2) <- source
  (d1, d2) <- inspect2 (copy1, copy2) (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)



-- Main program
main :: IO ()
main = do
  printStats "(State, Nothing)" 10000 runTrial