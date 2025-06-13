module Concur.PaperOthing where

import Config
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


-- communication channels for two observables
type Pixel = (Property, Decision)
type ChannelT = TVar (Maybe Pixel)
type M = ReaderT ChannelT IO


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


sys :: Property -> M Decision
sys prop = do
  mine1 <- renderPixel prop -- primary rendering (mandatory)
  mine2 <- renderPixel prop -- secondary rendering (eagerly)
  channel <- ask
  yours <- liftIO $ readTVarIO channel
  protocol yours mine1 mine2


-- Paper Excutable|Appearance|For Us
-- alias : Reference
type Copy = Property -> M Decision


-- Source gives a copy of paper to both reviewers
source :: IO (Copy, Copy)
source = return (sys, sys)


inspect1 :: Copy -> Property -> IO Decision
inspect1 copy prop = do
  hvar <- newTVarIO Nothing
  runReaderT (copy prop) hvar


inspect2 :: (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect2 (copy1, copy2) (prop1, prop2) = do
  hvar <- newTVarIO Nothing
  (dec1, dec2) <- concurrently 
    (runReaderT (copy1 prop1) hvar)
    (runReaderT (copy2 prop2) hvar)
  return (dec1, dec2)


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  -- paper <- randomPaper
  (copy1, copy2) <- source
  (d1, d2) <- inspect2 (copy1, copy2) (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)


main :: IO ()
main = do
  printStats "(Concurrency, Othing)" 10000 runTrial


