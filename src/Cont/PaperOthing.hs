module Cont.PaperOthing (sys, run2) where

import Config
import Context2
import RandomUtils
import Cont.EffectT
import Control.Monad.Cont
import Control.Concurrent.Async


-- Intuition: if thePaper is indeed nothing, then we don't really need a blueprint.
-- instead, we can just render a <partial paper>, or a <pixel> on-the-fly.

type Pixel = (Property, Decision)
type M = YieldT Pixel Pixel Decision IO
-- in this model HiddenVar is purely redundant
-- it can be any type
type HiddenVar = ()


type Copy = Property -> M Decision


src :: IO HiddenVar
src = return ()


-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> M Pixel
renderPixel prop = do d <- liftIO randomDecision; return (prop, d)


-- render a paper with an ad hoc decision for just one property
protocol :: Pixel -> Pixel -> Pixel -> Decision
protocol py p1 (_, dd2) = 
  case (py, p1) of
    ((propy, ddy), (propm, dd1)) | propy == propm -> ddy
    ((propy, ddy), (propm, dd1)) | ddy == dd1 -> dd2
    ((propy, ddy), (propm, dd1)) | ddy /= dd1 -> dd1
    _ -> error "internal bug."


sys :: Copy
sys prop = do
  mine1 <- renderPixel prop -- primary rendering
  mine2 <- renderPixel prop -- secondary rendering
  yours <- yield mine1 -- receive yours; send mine1
  return (protocol yours mine1 mine2)


-- TODO: it is possible to add a program for <Judge> as a mediator
-- now the inspect function serves as <Judge> implicitly


run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = do
  Context (Susp pixel1 k1, Susp pixel2 k2) <- sequence $ fmap runYieldT $ cs <*> ps
  Result dec1 <- k1 pixel2
  Result dec2 <- k2 pixel1
  let proc1 = return $ Context (dec1, snd pixel2)
      proc2 = return $ Context (snd pixel1, dec2)
  winner <- race proc1 proc2  -- let them race!
  case winner of
    Left res  -> return res
    Right res -> return res


{--
inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = do
  Susp pixel1 k1 <- runYieldT $ copy1 prop1
  Susp pixel2 k2 <- runYieldT $ copy2 prop2
  Result dec1 <- k1 pixel2
  Result dec2 <- k2 pixel1
  let proc1 = return (dec1, snd pixel2)
      proc2 = return (snd pixel1, dec2)
  winner <- race proc1 proc2  -- let them race!
  case winner of
    Left res  -> return res
    Right res -> return res



runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = return ()
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr



-- Main program
main :: IO ()
main = do
  printStats "(Continuation, Othing)" 8000 runTrial
--}



-- [TODO] explore connection to <call-by-need> vs.<eager computation> as in PL
---- currently the concept <by-need> is not explicitly realized in semantics


-- [TODO] need to accommodate this model with interpretations in physics
---- candidate theory: retrocausality


-- in Mermin's experiment, the detector flashes instantly the moment the particle arrives
-- while here there is suspension and synchronisation happenining according to the semantics