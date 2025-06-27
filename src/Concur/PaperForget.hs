{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Concur.PaperForget (
  sys1, sys2, run1, run1S, run2, run2S, run2A, run2AS, label
) where

import Config
import Context2
import Contextuality
import RandomUtils
import PaperCore
import Concur.MyLock (atomicIO)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


label :: String
label = "(Cuncurrency model -- Forget)"


type ChannelT = TVar Paper
type HiddenVar = ChannelT
type M = ReaderT HiddenVar IO


src :: IO HiddenVar
src = randomPaper >>= (\p -> newTVarIO p)


instance PaperCore M where

  getDecision prop = do
    channel <- ask
    paper <- liftIO $ readTVarIO channel
    case prop of
      Margins   -> return (margins paper)
      FontSize  -> return (fontSize paper)
      NumPages  -> return (numPages paper)

  putDecision prop d = do
    channel <- ask
    paper <- liftIO $ readTVarIO channel
    let newPaper = case prop of
          Margins  -> paper { margins = d }
          FontSize -> paper { fontSize = d }
          NumPages -> paper { numPages = d }
    liftIO $ atomically $ writeTVar channel newPaper


instance PaperForget M where


-- the main logic for quantum system <appearance>
-- TODO: same code as in  State.PaperForget
sys :: Copy M
sys prop = do
  d <- getDecisionF prop
  case d of
    Nothing -> renderDecision prop
    Just dd -> return dd


sys1 :: IO (Copy M)
sys1 = distribute1 sys

sys2 :: IO (Context (Copy M))
sys2 = distribute2 sys sys


instance Contextuality Context M HiddenVar where
  run1S s c ps = 
    mapConcurrently (\m -> atomicIO $ runReaderT m s) (fmap c ps)
  
  run2S s cs ps = -- still sequential, not using concurrency
    runReaderT (entangle $ cs <*> ps) s

  run2AS s cs ps =
    mapConcurrently (\m -> atomicIO $ runReaderT m s) (cs <*> ps)

  run1 c ps = do hvar <- src; run1S hvar c ps
  run2 cs ps = do hvar <- src; run2S hvar cs ps
  run2A cs ps = do hvar <- src; run2AS hvar cs ps