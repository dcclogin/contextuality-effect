module State.PaperNothing (
  bipartite, runfSeq, runfPar, runContextA, runContextP, label
) where

import Config
import Context2
import Contextuality
import RandomUtils
import PaperCore
import Control.Monad.State.Lazy


label :: String
label = "(State model -- Nothing)"


-- nonlocal hidden variable as state monad
-- type M = State Paper
type HiddenVar = Paper
type M = StateT HiddenVar IO


src :: IO HiddenVar
src = return thePaper

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


makeCopyN :: Int -> Copy M
makeCopyN n prop = getDecision prop >>= maybe (try n) return
  where try 0 = renderDecision >>= store
        try k = do
          d  <- renderDecision
          ok <- recallDecision prop (== d)
          if ok then try (k - 1) else store d
        store d = putDecision prop (Just d) >> return d


bipartite :: IO (Context (Copy M))
bipartite = distribute2 copy copy
