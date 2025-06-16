module Singleton.Nothing where

import Config
import Control.Monad.State.Lazy


type M = StateT Paper IO


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
  let newPaper = case prop of
		Margins   -> paper { margins = d }
		FontSize  -> paper { fontSize = d }
		NumPages  -> paper { numPages = d }
  put newPaper


-- render a random decision for a property
renderDecision :: Property -> M Decision
renderDecision prop = do
  dd <- liftIO randomDecision
  putDecision prop (Just dd)
  return dd


sys :: Property -> M Decision
sys = renderDecision


type Reference = Property -> M Decision

source1 :: IO Reference
source1 = return sys


inspect :: Paper -> Reference -> Property -> IO Decision
inspect paper ref prop = evalStateT (ref prop) paper


runTrial :: IO Decision
runTrial = do
  prop <- randomProperty
  -- paper <- randomPaper
  ref <- source1
  dec <- inspect thePaper ref prop
  return dec