module Concur.PaperForget (sys, run1, run2) where

import Config
import Context2
import RandomUtils
import Concur.MyLock (withLock)
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


-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


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
sys :: Copy
sys prop = do
  d <- getDecisionF prop
  case d of
    Nothing -> renderDecision prop
    Just dd -> return dd


sys1 :: IO Copy
sys1 = return sys

sys2 :: IO (Context Copy)
sys2 = return $ Context (sys, sys)


-- hiding HiddenVar and export
run1 :: Copy -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- src
  lock <- newTVarIO False
  mapConcurrently (\m -> withLock lock $ runReaderT m hvar) (fmap c ps)


-- hiding HiddenVar and export
run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- src
  lock <- newTVarIO False
  mapConcurrently (\m -> withLock lock $ runReaderT m hvar) (cs <*> ps)



{--
inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = do
  lock <- newTVarIO False
  (dec1, dec2) <- concurrently 
    (withLock lock $ runReaderT (copy1 prop1) hvar)
    (withLock lock $ runReaderT (copy2 prop2) hvar)
  return (dec1, dec2)


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = randomPaper >>= (\p -> newTVarIO p)
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr


main :: IO ()
main = do
  printStats "(Concurrency, Forget)" 10000 runTrial
--}