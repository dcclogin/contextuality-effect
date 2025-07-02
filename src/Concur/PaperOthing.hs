{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
module Concur.PaperOthing (
  bipartite, runfSeq, runfPar, runContextA, runContextP, label
) where

import Config
import Context2
import Contextuality
import RandomUtils
import PaperCore
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

runContextA :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextA cs ps = src >>= \s -> runfSeq s cs ps
runContextP :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextP cs ps = src >>= \s -> runfPar s cs ps


instance PaperCore M where
  putDecision prop (Just dec) = do
    channel <- ask
    liftIO $ atomically $ writeTVar channel (Just (prop, dec))
  putDecision _ Nothing = do
    channel <- ask
    liftIO $ atomically $ writeTVar channel Nothing

instance PaperOthing M where
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


copy :: Copy M
copy prop = do
  mine1 <- renderPixel prop -- primary rendering (mandatory)
  mine2 <- renderPixel prop -- secondary rendering (eagerly)
  yours <- getPixel
  protocol yours mine1 mine2


bipartite :: IO (Context (Copy M))
bipartite = distribute2 copy copy



