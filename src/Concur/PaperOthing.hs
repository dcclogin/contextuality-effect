module Concur.PaperOthing (sys, sys1, sys2, run1, run2, label) where

import Config
import Context2
import RandomUtils
import Concur.MyLock (withLock)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


label :: String
label = "(Cuncurrency model -- Othing)"


-- communication channels for two observables
type Pixel = (Property, Decision)
type ChannelT = TVar (Maybe Pixel)
type M = ReaderT ChannelT IO
-- type alias
type HiddenVar = ChannelT

-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


src :: IO HiddenVar
src = newTVarIO Nothing


-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> M Pixel
renderPixel prop = do d <- liftIO randomDecision; return (prop, d)


-- render a paper with an ad hoc decision for just one property
protocol :: Maybe Pixel -> Pixel -> Pixel -> M Decision
protocol py p1 (_, dec2) = case (py, p1) of
  (Nothing, (_, dec1)) -> do 
    channel <- ask
    liftIO $ atomically $ writeTVar channel (Just p1)
    return dec1
  (Just (propY, decY), (propM, dec1)) | propY == propM -> return decY
  (Just (propY, decY), (propM, dec1)) | decY == dec1 -> return dec2
  (Just (propY, decY), (propM, dec1)) | decY /= dec1 -> return dec1
  _ -> error "internal bug."


sys :: Copy
sys prop = do
  mine1 <- renderPixel prop -- primary rendering (mandatory)
  mine2 <- renderPixel prop -- secondary rendering (eagerly)
  channel <- ask
  yours <- liftIO $ readTVarIO channel
  protocol yours mine1 mine2


sys1 :: IO Copy
sys1 = return sys

sys2 :: IO (Context Copy)
sys2 = return $ Context (sys, sys)


{--
inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = do
  lock <- newTVarIO False
  (dec1, dec2) <- concurrently 
    (withLock lock $ runReaderT (copy1 prop1) hvar)
    (withLock lock $ runReaderT (copy2 prop2) hvar)
  return (dec1, dec2)
--}


-- hiding HiddenVar and export
run1 :: Copy -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- src
  lock <- newTVarIO False
  mapConcurrently (\m -> withLock lock $ runReaderT m hvar) (fmap c ps)


-- hiding HiddenVar and export
run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- src
  lock <- newTVarIO False
  mapConcurrently (\m -> withLock lock $ runReaderT m hvar) (cs <*> ps)


