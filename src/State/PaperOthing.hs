{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module State.PaperOthing (
  sys1, sys2, run1, run1S, run2, run2S, run2A, run2AS, label
) where

import Config
import Context2
import Contextuality
import RandomUtils
import PaperCore
import Control.Monad.State.Lazy


label :: String
label = "(State model -- Othing)"


type Pixel = (Property, Decision)
type HiddenVar = Maybe Pixel
type M = StateT HiddenVar IO


src :: IO HiddenVar
src = return Nothing


instance PaperCore M where

  getDecision prop = do
    pixel <- get
    return $ case pixel of
      Just (p, dec) | p == prop -> Just dec
      _ -> Nothing
  
  putDecision prop (Just dec) = put (Just (prop, dec))
  putDecision _ Nothing = put Nothing



-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> M Pixel
renderPixel prop = do d <- liftIO randomDecision; return (prop, d)

getPixel :: M (Maybe Pixel)
getPixel = get


-- render a paper with an ad hoc decision for just one property
protocol :: Maybe Pixel -> Pixel -> Pixel -> M Decision
protocol Nothing (prop1, dec1) _ = do
  putDecision prop1 (Just dec1)
  return dec1
protocol (Just (propY, decY)) (propM, dec1) (_, dec2) -- _ must be == propM
  | propY == propM = return decY
  | decY == dec1   = return dec2
  | otherwise      = return dec1


sys :: Copy M
sys prop = do
  mine1 <- renderPixel prop -- primary rendering (mandatory)
  mine2 <- renderPixel prop -- secondary rendering (eagerly)
  yours <- getPixel
  protocol yours mine1 mine2


sys1 :: IO (Copy M)
sys1 = distribute1 sys

sys2 :: IO (Context (Copy M))
sys2 = distribute2 sys sys


instance Contextuality Context M HiddenVar where
  run1S s c ps   = evalStateT (traverse c ps) s
  run2S s cs ps  = evalStateT (entangle $ cs <*> ps) s
  run2AS s cs ps = traverse (\m -> evalStateT m s) (cs <*> ps)
  run1 c ps      = do hvar <- src; run1S hvar c ps
  run2 cs ps     = do hvar <- src; run2S hvar cs ps
  run2A cs ps    = do hvar <- src; run2AS hvar cs ps