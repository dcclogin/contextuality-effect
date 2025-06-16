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


type Copy = Property -> IO (M Decision)


-- in this model HiddenVar is purely redundant
-- it can be any type
type HiddenVar = ()


inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = do
  Susp pixel1 k1 <- copy1 prop1
  Susp pixel2 k2 <- copy2 prop2
  let Result dd1 = k1 pixel2
      Result dd2 = k2 pixel1 in
    return (dd1, snd pixel2)
        -- (snd pixel1, dd2)


execute :: Trial HiddenVar Copy -> IO (Outcome, Outcome)
execute tr = do
  hvar <- source tr
  (copy1, copy2) <- copies tr
  prop1 <- choice $ fst $ reviewers tr
  prop2 <- choice $ snd $ reviewers tr
  (dec1, dec2) <- inspect hvar (copy1, copy2) (prop1, prop2)
  return (Outcome prop1 dec1, Outcome prop2 dec2)


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = return ()
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
      } 
  getAgreement $ execute tr



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