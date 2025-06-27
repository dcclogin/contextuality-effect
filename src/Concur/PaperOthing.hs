{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}
module Concur.PaperOthing (
  sys1, sys2, run1, run1S, run2, run2S, run2A, run2AS, label
) where

import Config
import Context2
import Contextuality
import RandomUtils
import PaperCore
import Concur.MyLock (atomicIO)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


label :: String
label = "(Cuncurrency model -- Othing)"


type Pixel = (Property, Decision)
type ChannelT = TVar (Maybe Pixel)
type HiddenVar = ChannelT
type M = ReaderT HiddenVar IO


src :: IO HiddenVar
src = newTVarIO Nothing


instance PaperCore M where

  putDecision prop (Just dec) = do
    channel <- ask
    liftIO $ atomically $ writeTVar channel (Just (prop, dec))
  putDecision _ Nothing = do
    channel <- ask
    liftIO $ atomically $ writeTVar channel Nothing



-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> M Pixel
renderPixel prop = do d <- liftIO randomDecision; return (prop, d)

getPixel :: M (Maybe Pixel)
getPixel = do channel <- ask; liftIO $ readTVarIO channel


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
  run1S s c ps = 
    mapConcurrently (\m -> atomicIO $ runReaderT m s) (fmap c ps)
  
  run2S s cs ps = -- still sequential, not using concurrency
    runReaderT (entangle $ cs <*> ps) s

  run2AS s cs ps =
    mapConcurrently (\m -> atomicIO $ runReaderT m s) (cs <*> ps)

  run1 c ps = do hvar <- src; run1S hvar c ps
  run2 cs ps = do hvar <- src; run2S hvar cs ps
  run2A cs ps = do hvar <- src; run2AS hvar cs ps


