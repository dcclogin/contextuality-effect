module State.PaperForget where

import Config
import System.Random
import Control.Monad.State.Lazy


-- probablistic contextuality: cannot have 3 determinate properties at the same time
-- strong contextuality: will lead to formal contradiction, global inconsistency

-- may be compared to Spekken's toy model and <knowledge-balance principle>


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
  let updatedPaper = case prop of
		Margins   -> paper { margins = d }
		FontSize  -> paper { fontSize = d }
		NumPages  -> paper { numPages = d }
  put updatedPaper


-- render a random decision for a specific property
renderDecision :: Property -> M Decision
renderDecision prop = do
  dd <- liftIO randomDecision
  putDecision prop (Just dd)
  return dd


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


-- the main logic for quantum system <appearance>
sys :: Property -> M Decision
sys prop = do
  d <- getDecisionF prop
  case d of
    Nothing -> renderDecision prop
    Just dd -> return dd


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
          source = randomPaper
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
      } 
  getAgreement $ execute tr


-- Main program
main :: IO ()
main = do
  printStats "(State, Forget)" 10000 runTrial


-- [TODO] connection to <call-by-reference> as in PL