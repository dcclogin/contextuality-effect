module State.PaperOthing where

import Config
import System.Random
import Control.Monad.State.Lazy


type Pixel = (Property, Decision)
type HiddenVar = Maybe Pixel
type M = StateT HiddenVar IO


-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> M Pixel
renderPixel prop = do d <- liftIO randomDecision; return (prop, d)


-- render a paper with an ad hoc decision for just one property
protocol :: Maybe Pixel -> Pixel -> Pixel -> M Decision
protocol py p1 (_, dec2) = case (py, p1) of
  (Nothing, (_, dec1)) -> do put (Just p1); return dec1
  (Just (propY, decY), (propM, dec1)) | propY == propM -> return decY
  (Just (propY, decY), (propM, dec1)) | decY == dec1 -> return dec2
  (Just (propY, decY), (propM, dec1)) | decY /= dec1 -> return dec1
  _ -> error "internal bug."


sys :: Property -> M Decision
sys prop = do
  mine1 <- renderPixel prop -- primary rendering (mandatory)
  mine2 <- renderPixel prop -- secondary rendering (eagerly)
  yours <- get
  protocol yours mine1 mine2


-- bipartite system
(⨷) :: (Property -> M Decision) 
    -> (Property -> M Decision) 
    -> (Property, Property) -> M (Decision, Decision)
(sys1 ⨷ sys2) (prop1, prop2) = do
  d1 <- sys1 prop1
  d2 <- sys2 prop2
  return (d1, d2)


-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = do
  let m = (copy1 ⨷ copy2) (prop1, prop2)
  evalStateT m hvar


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = return Nothing
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr


-- Main program
main :: IO ()
main = do
  printStats "(State, Othing)" 12345 runTrial
