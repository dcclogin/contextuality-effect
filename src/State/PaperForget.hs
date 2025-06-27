{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
module State.PaperForget (
  sys1, sys2, run1, run1S, run2, run2S, run2A, run2AS, label
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

run1   :: Copy M -> Context Property -> IO (Context Decision)
run1 c ps = src >>= \s -> run1S s c ps
run2   :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2 cs ps = src >>= \s -> run2S s cs ps
run2A  :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2A cs ps = src >>= \s -> run2AS s cs ps


instance PaperCore M where
  getDecision prop = do
    paper <- get
    case prop of
      Margins  -> return (margins paper)
      FontSize -> return (fontSize paper)
      NumPages -> return (numPages paper)
  putDecision prop d = do
    paper <- get
    case prop of
      Margins  -> put paper { margins = d }
      FontSize -> put paper { fontSize = d }
      NumPages -> put paper { numPages = d }
  renderDecision = liftIO randomDecision

instance PaperForget M where



-- the main logic for quantum system <appearance>
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



{--
partialMeasureL :: Property -> Context (Copy M) -> M (Decision, Copy M)
partialMeasureL prop (Context (copyL, _copyR)) = do
  d <- copyL prop
  newState <- get
  let updated = \p -> evalStateT (copyL p) newState
  return (d, updated)
--}




-- [TODO] connection to <call-by-reference> as in PL
-- this is an causal model
-- if we set the source to emit only (PPP), we can tell the causal flow
