{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
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

instance PaperForget M where


-- the main logic for quantum system <appearance>
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
  run1S s c ps   = evalStateT (traverse c ps) s
  run2S s cs ps  = evalStateT (entangle $ cs <*> ps) s
  run2AS s cs ps = traverse (\m -> evalStateT m s) (cs <*> ps)
  run1 c ps      = do hvar <- src; run1S hvar c ps
  run2 cs ps     = do hvar <- src; run2S hvar cs ps
  run2A cs ps    = do hvar <- src; run2AS hvar cs ps



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


{--
-- no concept of quantum states and observables as projectors
-- is it still possible to express <commutativity>?
ltor, rtol :: Context Property -> IO HiddenVar -> IO HiddenVar
ltor = flip $ foldl f
  where
    f :: IO HiddenVar -> Property -> IO HiddenVar
    f s p = do hvar <- s; execStateT (sys p) hvar
rtol = flip $ foldr f
  where
    f :: Property -> IO HiddenVar -> IO HiddenVar
    f p s = do hvar <- s; execStateT (sys p) hvar
--}
