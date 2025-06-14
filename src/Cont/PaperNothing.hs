module Cont.PaperNothing where

import Config
import System.Random
import Cont.Effect
import Control.Monad.Cont


-- [TODO]: instead of wrapping a layer of IO on top of M,
-- define a monad transformer to combine it with IO effects
type M = Iterator Paper Paper
-- type M = Iterator (Paper, Property) (Paper, Property)
-- type M = Iterator (Property, Decision) (Property, Decision)


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


{--
sys :: Property -> M Decision
sys prop = runYield $ do
  mine <- renderPaper prop
  yours <- yield mine
  return $ compromise ...
--}

sys :: Property -> IO (M Decision)
sys prop = do
  mine1 <- renderPaper prop -- primary rendering
  mine2 <- renderPaper prop -- secondary rendering
  return $ runYield $ do
    -- receive yours from, and send mine1 in the <channel>
    yours <- yield mine1
    return $ compromise prop mine1 mine2 yours


type Copy = Property -> IO (M Decision)


source :: IO (Copy, Copy)
source = return (sys, sys)


-- TODO: bipartite system

-- inspect2 doesn't even need a input paper
inspect2 :: (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect2 (copy1, copy2) (prop1, prop2) = do
  Susp paper1 k1 <- copy1 prop1
  Susp paper2 k2 <- copy2 prop2
  let Result dd1 = k1 paper2
      Result dd2 = k2 paper1 in
    return (dd1, extractDecision paper2)
        -- (extractDecision paper1, dd2)
  where
    extractDecision :: Paper -> Decision
    extractDecision paper = case (margins paper, fontSize paper, numPages paper) of
      (Just dd, _, _) -> dd
      (_, Just dd, _) -> dd
      (_, _, Just dd) -> dd
      _              -> error "internal bug."


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  (copy1, copy2) <- source
  (d1, d2) <- inspect2 (copy1, copy2) (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)
      

-- Main program
main :: IO ()
main = do
  printStats "(Continuation, Nothing)" 10000 runTrial

  


