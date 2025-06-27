{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
module Concur.PaperNothing (
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
label = "(Cuncurrency model -- Nothing)"


type ChannelT = TVar Paper
type HiddenVar = ChannelT
type M = ReaderT HiddenVar IO


src :: IO HiddenVar
src = newTVarIO thePaper

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

instance PaperNothing M where


-- decisions are rendered <by need>
-- TODO: same code as State.PaperNothing
sys :: Copy M
sys prop = do
  d <- getDecision prop
  case d of
    Just dec -> return dec
    Nothing -> do
      dec <- pick 1 (repeat renderDecision)
      putDecision prop (Just dec)
      return dec
  where
    pick :: Int -> [M Decision] -> M Decision
    pick 0 (d:ds) = d
    pick n (d:ds) = do
      dec <- d
      ok <- crecallDecision prop (== dec)
      if not ok then return dec else pick (n - 1) ds


sys1 :: IO (Copy M)
sys1 = distribute1 sys

sys2 :: IO (Context (Copy M))
sys2 = distribute2 sys sys

