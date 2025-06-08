module Cont.PaperOthing where

import Config
import System.Random
import Cont.Effect
import Control.Monad.Cont


-- blueprint of all papers rendered on-the-fly
-- thePaper :: Paper
-- thePaper = Paper Nothing Nothing Nothing

-- Intuition: if thePaper is indeed nothing, then we don't really need a blueprint.
-- instead, we can just render a <partial paper>, or a <pixel> on-the-fly.

type Pixel = (Property, Decision)
type M = Iterator Pixel Pixel


-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> IO Pixel
renderPixel prop = do d <- randomDecision; return (prop, d)


-- render a paper with an ad hoc decision for just one property
protocol :: Pixel -> Pixel -> Pixel -> Decision
protocol py p1 (_, dd2) = 
  case (py, p1) of
    ((propy, ddy), (propm, dd1)) | propy == propm -> ddy
    ((propy, ddy), (propm, dd1)) | ddy == dd1 -> dd2
    ((propy, ddy), (propm, dd1)) | ddy /= dd1 -> dd1
    _ -> error "internal bug."


sys :: Property -> IO (M Decision)
sys prop = do
  mine1 <- renderPixel prop -- primary rendering
  mine2 <- renderPixel prop -- secondary rendering
  return $ runYield $ do
    yours <- yield mine1 -- receive yours; send mine1
    return $ protocol yours mine1 mine2


inspect2 :: (Property, Property) -> IO (Decision, Decision)
inspect2 (prop1, prop2) = do
  Susp pixel1 k1 <- sys prop1
  Susp pixel2 k2 <- sys prop2
  let Result dd1 = k1 pixel2
      Result dd2 = k2 pixel1 in
    return (dd1, snd pixel2)
        -- (snd pixel1, dd2)


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  -- paper <- randomPaper
  (d1, d2) <- inspect2 (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)



-- Main program
main :: IO ()
main = do
  printStats "(Continuation, Othing)" 8000 runTrial



-- [TODO] explore connection to <call-by-need> vs.<eager computation> as in PL
---- currently the concept <by-need> is not explicitly realized in semantics


-- [TODO] need to accommodate this model with interpretations in physics
---- candidate theory: retrocausality


-- in Mermin's experiment, the detector flashes instantly the moment the particle arrives
-- while here there is suspension and synchronisation happenining according to the semantics