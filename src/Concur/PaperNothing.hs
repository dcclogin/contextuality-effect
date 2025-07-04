module Concur.PaperNothing (
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
label = "(Cuncurrency model -- Nothing)"


type ChannelT = TVar Paper
type HiddenVar = ChannelT
type M = ReaderT HiddenVar IO


src :: IO HiddenVar
src = newTVarIO thePaper

runContextA :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextA cs ps = src >>= \s -> runfSeq s cs ps
runContextP :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextP cs ps = src >>= \s -> runfPar s cs ps


copy :: Copy M
copy prop = getDecision prop >>= maybe gen return
  where gen = do
          d  <- renderDecision
          ok <- recallDecision prop (== d)
          d  <- if ok then renderDecision else return d
          putDecision prop (Just d) >> return d


-- decisions are rendered <by need>
-- TODO: same code as State.PaperNothing
copyN :: Int -> Copy M
copyN n prop = getDecision prop >>= maybe gen return
  where
    gen = attempt n
    attempt :: Int -> M Decision
    attempt n = do
      dec <- renderDecision
      ok  <- recallDecision prop (== dec)
      if not ok || n == 0
        then putDecision prop (Just dec) >> return dec
        else attempt (n - 1)



bipartite :: IO (Context (Copy M))
bipartite = distribute2 (copyN 1) (copyN 1)

