module Cont.PaperNothing (sys, sys1, sys2, run1, run2, label) where

import Config
import Context2
import RandomUtils
import Cont.EffectT
import Control.Monad.Cont
import Control.Concurrent.Async


label :: String
label = "(Continuation model -- Nothing)"


-- [TODO]: instead of wrapping a layer of IO on top of M,
-- define a monad transformer to combine it with IO effects
type M = YieldT Paper Paper Decision IO
-- type alias
type HiddenVar = Paper


src :: IO HiddenVar
src = return thePaper


-- render a paper with an ad hoc decision for just one property
renderPaper :: Property -> M Paper
renderPaper prop = do
  d <- liftIO randomDecision
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


sys :: Copy M
sys prop = do
  mine1 <- renderPaper prop -- primary rendering
  mine2 <- renderPaper prop -- secondary rendering
  -- receive yours from, and send mine1 in the <channel>
  yours <- yield mine1
  return (compromise prop mine1 mine2 yours)


sys1 :: IO (Copy M)
sys1 = return sys

sys2 :: IO (Context (Copy M))
sys2 = return $ Context (sys, sys)


-- workaround
extractDecision :: Paper -> Decision
extractDecision paper = case (margins paper, fontSize paper, numPages paper) of
  (Just dd, _, _) -> dd
  (_, Just dd, _) -> dd
  (_, _, Just dd) -> dd
  _               -> error "internal bug."


run1 :: Copy M -> Context Property -> IO (Context Decision)
run1 c ps = do
  Context (Susp paper1 k1, Susp paper2 k2) <- traverse runYieldT $ fmap c ps
  Result dec1 <- k1 paper2
  Result dec2 <- k2 paper1
  let proc1 = return $ Context (dec1, extractDecision paper2)
      proc2 = return $ Context (extractDecision paper1, dec2)
  winner <- race proc1 proc2  -- let them race!
  case winner of
    Left res  -> return res
    Right res -> return res


run2 :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2 cs ps = do
  Context (Susp paper1 k1, Susp paper2 k2) <- traverse runYieldT $ cs <*> ps
  Result dec1 <- k1 paper2
  Result dec2 <- k2 paper1
  let proc1 = return $ Context (dec1, extractDecision paper2)
      proc2 = return $ Context (extractDecision paper1, dec2)
  winner <- race proc1 proc2  -- let them race!
  case winner of
    Left res  -> return res
    Right res -> return res
