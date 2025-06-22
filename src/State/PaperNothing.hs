module State.PaperNothing (sys, run1, run2) where

import Config
import Context2
import RandomUtils
import Control.Monad.State.Lazy


-- nonlocal hidden variable as state monad
-- type M = State Paper
type HiddenVar = Paper
type M = StateT HiddenVar IO

-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


src :: IO HiddenVar
src = return thePaper


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
sys :: Copy
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


reifyEffect :: M (Context Decision) -> HiddenVar -> IO (Context Decision)
reifyEffect = evalStateT


-- hiding HiddenVar and export
run1 :: Copy -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- src
  reifyEffect (traverse c ps) hvar


-- hiding HiddenVar and export
run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- src
  reifyEffect (sequence $ cs <*> ps) hvar



{--
-- alternative: runStateT
inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) =
  let m = (copy1 ⨷ copy2) (prop1, prop2) in 
    evalStateT m hvar


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = return thePaper
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr


-- Main program
main :: IO ()
main = do
  printStats "(State, Nothing)" 10000 runTrial
--}