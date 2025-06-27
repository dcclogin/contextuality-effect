{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
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

run1   :: Copy M -> Context Property -> IO (Context Decision)
run1 c ps = src >>= \s -> run1S s c ps
run2   :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2 cs ps = src >>= \s -> run2S s cs ps
run2A  :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2A cs ps = src >>= \s -> run2AS s cs ps


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
  renderDecision = liftIO randomDecision

instance PaperForget M where


-- the main logic for quantum system <appearance>
-- TODO: same code as in  State.PaperForget
sys :: Copy M
sys prop = do
  d <- getDecisionF prop
  case d of
    Just dec -> return dec
    Nothing -> do
      dec <- renderDecision
      putDecision prop (Just dec)
      return dec


sys1 :: IO (Copy M)
sys1 = distribute1 sys

sys2 :: IO (Context (Copy M))
sys2 = distribute2 sys sys
