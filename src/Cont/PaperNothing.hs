module Cont.PaperNothing where

import Config
import RandomUtils
import Cont.EffectT
import Control.Monad.Cont
import Control.Concurrent.Async


-- [TODO]: instead of wrapping a layer of IO on top of M,
-- define a monad transformer to combine it with IO effects
type M = YieldT Paper Paper Decision IO
-- type alias
type HiddenVar = Paper
type Copy = Property -> M Decision


-- render a paper with an ad hoc decision for just one property
renderPaper :: Property -> IO Paper
renderPaper prop = do
  d <- randomDecision
  let paper = case prop of
		Margins   -> thePaper { margins = Just d }
		FontSize  -> thePaper { fontSize = Just d }
		NumPages  -> thePaper { numPages = Just d }
  return paper


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


-- check if any other properties have made a specific decision 
crecallDecision :: Paper -> Property -> (Maybe Decision -> Bool) -> Bool
crecallDecision paper Margins pred =
  pred (fontSize paper) || pred (numPages paper)
crecallDecision paper FontSize pred =
  pred (margins paper) || pred (numPages paper)
crecallDecision paper NumPages pred =
  pred (margins paper) || pred (fontSize paper)


-- the protocol
compromise :: Property -> Paper -> Paper -> Paper -> Decision
compromise prop m1 m2 y = 
  let d1 = getDecision m1 prop
      d2 = getDecision m2 prop
      dy = getDecision y prop in
  case (d1, dy, d2) of
    (Just dd1, Just ddy, _) -> ddy
    (Just dd1, Nothing, Just dd2) | crecallDecision y prop (== d1) -> dd2
    (Just dd1, Nothing, _) -> dd1
    _ -> error "internal bug."   


sys :: Copy
sys prop = do
  mine1 <- liftIO $ renderPaper prop -- primary rendering
  mine2 <- liftIO $ renderPaper prop -- secondary rendering
  -- receive yours from, and send mine1 in the <channel>
  yours <- yield mine1
  return (compromise prop mine1 mine2 yours)


inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = do
  Susp paper1 k1 <- runYieldT $ copy1 prop1
  Susp paper2 k2 <- runYieldT $ copy2 prop2
  Result dd1 <- k1 paper2
  Result dd2 <- k2 paper1
  let proc1 = return (dd1, extractDecision paper2)
      proc2 = return (extractDecision paper1, dd2)
  winner <- race proc1 proc2  -- let them race!
  case winner of
    Left res  -> return res
    Right res -> return res
  where
    extractDecision :: Paper -> Decision
    extractDecision paper = case (margins paper, fontSize paper, numPages paper) of
      (Just dd, _, _) -> dd
      (_, Just dd, _) -> dd
      (_, _, Just dd) -> dd
      _               -> error "internal bug."


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = return thePaper
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr
      

-- Main program
main :: IO ()
main = do
  printStats "(Continuation, Nothing)" 10000 runTrial

  


