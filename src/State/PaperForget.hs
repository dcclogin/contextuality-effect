module State.PaperForget (
  bipartite, runfSeq, runfPar, runContextA, runContextP, label
) where

import Config
import Context2
import Contextuality
import PaperCore
import RandomUtils
import Control.Monad.State.Lazy


label :: String
label = "(State model -- Forget)"


-- probablistic contextuality: cannot have 3 determinate properties at the same time
-- strong contextuality: will lead to formal contradiction, global inconsistency

-- may be compared to Spekken's toy model and <knowledge-balance principle>


-- nonlocal hidden variable as state monad
type HiddenVar = Paper
type M = StateT HiddenVar IO


src :: IO HiddenVar
src = randomPaper

runContextA :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextA cs ps = src >>= \s -> runfSeq s cs ps
runContextP :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextP cs ps = src >>= \s -> runfPar s cs ps


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
