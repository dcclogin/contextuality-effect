module State.PaperOthing (sys, run1, run2, Copy) where

import Config
import Context2
import System.Random
import Control.Monad.State.Lazy


type Pixel = (Property, Decision)
type HiddenVar = Maybe Pixel
type M = StateT HiddenVar IO

-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


src :: IO HiddenVar
src = return Nothing


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


sys :: Copy
sys prop = do
  mine1 <- renderPixel prop -- primary rendering (mandatory)
  mine2 <- renderPixel prop -- secondary rendering (eagerly)
  yours <- get
  protocol yours mine1 mine2


{--
getObs :: Context Copy -> Context Property -> Context (M Decision)
getObs cs ps = cs <*> ps
--}


reifyEffect :: M (Context Decision) -> HiddenVar -> IO (Context Decision)
reifyEffect = evalStateT


-- hiding HiddenVar and export
run1 :: Copy -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- src
  reifyEffect (traverse c ps) hvar


-- hiding HiddenVar and export
run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- src
  reifyEffect (sequence $ cs <*> ps) hvar
