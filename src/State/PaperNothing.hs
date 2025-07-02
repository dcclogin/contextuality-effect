{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
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
copy :: Copy M
copy prop = do
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


bipartite :: IO (Context (Copy M))
bipartite = distribute2 copy copy
