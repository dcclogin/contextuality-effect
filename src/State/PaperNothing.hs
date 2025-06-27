{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
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

instance PaperNothing M where



-- decisions are rendered <by need>
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
