module State.PaperNothing where

import Config
import System.Random
import Control.Monad.State.Lazy


-- nonlocal hidden variable as state monad
-- type M = State Paper
type HiddenVar = Paper
type M = StateT HiddenVar IO


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


-- alternative: runStateT
inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) =
  let m = (copy1 ⨷ copy2) (prop1, prop2) in 
    evalStateT m hvar


execute :: Trial HiddenVar Copy -> IO (Outcome, Outcome)
execute tr = do
  hvar <- source tr
  (copy1, copy2) <- copies tr
  prop1 <- choice $ fst $ reviewers tr
  prop2 <- choice $ snd $ reviewers tr
  (dec1, dec2) <- inspect hvar (copy1, copy2) (prop1, prop2)
  return (Outcome prop1 dec1, Outcome prop2 dec2)


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = return thePaper
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
      } 
  getAgreement $ execute tr


-- Main program
main :: IO ()
main = do
  printStats "(State, Nothing)" 10000 runTrial