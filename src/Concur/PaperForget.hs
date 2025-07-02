{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
module Concur.PaperForget (
  bipartite, runfSeq, runfPar, runContextA, runContextP, label
) where

import Config
import Context2
import Contextuality
import RandomUtils
import PaperCore
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

runContextA :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextA cs ps = src >>= \s -> runfSeq s cs ps
runContextP :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextP cs ps = src >>= \s -> runfPar s cs ps


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
copy :: Copy M
copy prop = do
  d <- getDecisionF prop
  case d of
    Just dec -> return dec
    Nothing -> do
      dec <- renderDecision
      putDecision prop (Just dec)
      return dec


bipartite :: IO (Context (Copy M))
bipartite = distribute2 copy copy
