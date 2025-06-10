module Concur.PaperForget where


import Config
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
-- import Control.Monad.State.Lazy
import Control.Monad.Reader


-- communication channels for two observables
type ChannelT = TVar Paper
type M = ReaderT ChannelT IO


getDecision :: Property -> M (Maybe Decision)
getDecision prop = do
  channel <- ask
  paper <- liftIO $ readTVarIO channel
  case prop of
    Margins   -> return (margins paper)
    FontSize  -> return (fontSize paper)
    NumPages  -> return (numPages paper)

  
putDecision :: Property -> Maybe Decision -> M ()
putDecision prop d = do
  channel <- ask
  paper <- liftIO $ readTVarIO channel
  let newPaper = case prop of
		Margins   -> paper { margins = d }
		FontSize  -> paper { fontSize = d }
		NumPages  -> paper { numPages = d }
  liftIO $ atomically $ writeTVar channel newPaper


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
    Nothing -> do
      dd <- liftIO randomDecision
      putDecision prop (Just dd)
      return dd
    Just dd -> return dd


-- bipartite system


-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


-- Source gives the same copy to both reviewers
source :: IO (Copy, Copy)
source = return (sys, sys)


inspect1 :: Paper -> Copy -> Property -> IO Decision
inspect1 paper copy prop = do
  hvar <- newTVarIO paper
  runReaderT (copy prop) hvar


inspect2 :: Paper -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect2 paper (copy1, copy2) (prop1, prop2) = do
  hvar <- newTVarIO paper  -- "initialize hidden variable"
  (dec1, dec2) <- concurrently 
    (runReaderT (copy1 prop1) hvar)
    (runReaderT (copy2 prop2) hvar)
  return (dec1, dec2)


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

main :: IO ()
main = do
  printStats "(Concurrency, Forget)" 10000 runTrial