module Concur.PaperForget (
  sys, sys1, sys2, run1, run2, run2A, label
) where

import Config
import Context2
import RandomUtils
import Concur.MyLock (atomicIO, withLock)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


label :: String
label = "(Cuncurrency model -- Forget)"


-- communication channels for two observables
type ChannelT = TVar Paper
type M = ReaderT ChannelT IO
-- type alias
type HiddenVar = ChannelT


src :: IO HiddenVar
src = randomPaper >>= (\p -> newTVarIO p)


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


-- render a random decision for a property
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
sys :: Copy M
sys prop = do
  d <- getDecisionF prop
  case d of
    Nothing -> renderDecision prop
    Just dd -> return dd


sys1 :: IO (Copy M)
sys1 = return sys

sys2 :: IO (Context (Copy M))
sys2 = return $ Context (sys, sys)


-- hiding HiddenVar and export
run1 :: Copy M -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- src
  mapConcurrently (\m -> atomicIO $ runReaderT m hvar) (fmap c ps)


-- still sequential, not using concurrency
run2 :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- src
  runReaderT (entangle $ cs <*> ps) hvar


-- hiding HiddenVar and export
run2A :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2A cs ps = do
  hvar <- src
  mapConcurrently (\m -> atomicIO $ runReaderT m hvar) (cs <*> ps)