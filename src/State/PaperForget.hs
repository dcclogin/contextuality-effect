{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module State.PaperForget (
  sys1, sys2, run1, run1S, run2, run2S, run2A, run2AS, label
) where

import Config
import Context2
import Contextuality
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


getDecision :: Property -> M (Maybe Decision)
getDecision prop = do
  paper <- get
  case prop of
    Margins   -> return (margins paper)
    FontSize  -> return (fontSize paper)
    NumPages  -> return (numPages paper)

  
putDecision :: Property -> Maybe Decision -> M ()
putDecision prop d = do
  paper <- get
  let updatedPaper = case prop of
		Margins   -> paper { margins = d }
		FontSize  -> paper { fontSize = d }
		NumPages  -> paper { numPages = d }
  put updatedPaper


-- render a random decision for a specific property
renderDecision :: Property -> M Decision
renderDecision prop = do
  dd <- liftIO randomDecision
  putDecision prop (Just dd)
  return dd


-- unconditional forget
forgetDecision :: Property -> M ()
forgetDecision prop = putDecision prop Nothing


-- conditional forget
cforgetDecision :: Property -> (Maybe Decision -> Bool) -> M ()
cforgetDecision prop pred = do
  d <- getDecision prop
  if pred d then forgetDecision prop else return ()


-- forgetful-get
getDecisionF :: Property -> M (Maybe Decision)
getDecisionF Margins = do
  m <- getDecision Margins
  cforgetDecision FontSize (== m)
  cforgetDecision NumPages (== m)
  return m
getDecisionF FontSize = do
  m <- getDecision FontSize
  cforgetDecision Margins (== m)
  cforgetDecision NumPages (== m)
  return m
getDecisionF NumPages = do
  m <- getDecision NumPages
  cforgetDecision Margins (== m)
  cforgetDecision FontSize (== m)
  return m


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


instance Contextuality HiddenVar M where
  run1S s c ps   = evalStateT (traverse c ps) s
  run2S s cs ps  = evalStateT (entangle $ cs <*> ps) s
  run2AS s cs ps = traverse (\m -> evalStateT m s) (cs <*> ps)
  run1 c ps      = do hvar <- src; run1S hvar c ps
  run2 cs ps     = do hvar <- src; run2S hvar cs ps
  run2A cs ps    = do hvar <- src; run2AS hvar cs ps



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
