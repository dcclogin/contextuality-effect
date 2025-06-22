module Concur.PaperNothing (sys, run1, run2) where

import Config
import Context2
import RandomUtils
import Concur.MyLock (withLock)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


-- communication channels for two observables
type ChannelT = TVar Paper
type M = ReaderT ChannelT IO
-- type alias
type HiddenVar = ChannelT

-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


src :: IO HiddenVar
src = newTVarIO thePaper


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
          source = newTVarIO thePaper
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr


main :: IO ()
main = do
  printStats "(Concurrency, Nothing)" 10000 runTrial
--}