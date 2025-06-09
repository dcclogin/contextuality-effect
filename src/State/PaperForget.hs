module State.PaperForget where

import Config
import System.Random
import Control.Monad.State.Lazy


-- cannot have 3 determinate properties at the same time
-- may be compared to Spekken's toy model and <knowledge-balance principle>


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


-- Paper Excutable|Appearance|For Us
type Copy = Property -> M Decision


-- Source gives the same paper to both reviewers
source :: IO (Copy, Copy)
source = return (sys, sys)


inspect1 :: Paper -> Copy -> Property -> IO Decision
inspect1 paper copy prop = evalStateT (copy prop) paper


inspect2 :: Paper -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect2 paper (copy1, copy2) (prop1, prop2) =
	let m = (copy1 ⨷ copy2) (prop1, prop2) in 
		evalStateT m paper


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  paper <- randomPaper
  (copy1, copy2) <- source
  (d1, d2) <- inspect2 paper (copy1, copy2) (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)



-- Main program
main :: IO ()
main = do
  printStats "(State, Forget)" 10000 runTrial


-- [TODO] connection to <call-by-reference> as in PL