{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module State.PaperNothing (
  sys1, sys2, run1, run1S, run2, run2S, run2A, run2AS, label
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


instance PaperNothing M where

{--
-- stream of random decisions for a property
-- TODO: integrate with Traversable
-- renderDecisions :: Property -> M [Decision]
-- renderDecisions prop = sequenceA $ repeat (renderDecision prop)
--}

-- decisions are rendered <by need>
sys :: Copy M
sys prop = do
  d <- getDecision prop
  case d of
    Just dec -> return dec
    Nothing -> do
      dec <- renderDecision prop
      b <- crecallDecision prop (== dec)
      if (not b)
        then return dec
        else renderDecision prop
        -- re-render if the same decision is already made for another property


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