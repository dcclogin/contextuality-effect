module Singleton.Something where

import Config


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


type Copy = Property -> Decision


cp :: Paper -> Copy
cp paper = \prop -> case getDecision paper prop of
  Just dec -> dec
  Nothing  -> error "internal bug."


inspect :: Copy -> Property -> IO Decision
inspect copy prop = return $ copy prop


runTrial :: IO Decision
runTrial = do
  prop <- randomProperty
  paper <- randomPaper
  dec <- inspect (cp paper) prop
  return dec