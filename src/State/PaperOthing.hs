{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
module State.PaperOthing (
  bipartite, runfSeq, runfPar, runContextA, runContextP, label
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

runContextA :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextA cs ps = src >>= \s -> runfSeq s cs ps
runContextP :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextP cs ps = src >>= \s -> runfPar s cs ps

instance PaperCore M where
  getDecision prop = do
    pixel <- get
    return $ case pixel of
      Just (p, dec) | p == prop -> Just dec
      _ -> Nothing
  
  putDecision prop (Just dec) = put (Just (prop, dec))
  putDecision _ Nothing = put Nothing

instance PaperOthing M where
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


copy :: Copy M
copy prop = do
  mine1 <- renderPixel prop -- primary rendering (mandatory)
  mine2 <- renderPixel prop -- secondary rendering (eagerly)
  yours <- getPixel
  protocol yours mine1 mine2


bipartite :: IO (Context (Copy M))
bipartite = distribute2 copy copy
