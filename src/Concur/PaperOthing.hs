module Concur.PaperOthing where

import Config
import Context2
import Concur.MyLock (withLock)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


-- communication channels for two observables
type Pixel = (Property, Decision)
type ChannelT = TVar (Maybe Pixel)
type M = ReaderT ChannelT IO
-- type alias
type HiddenVar = ChannelT


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



inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = do
  lock <- newTVarIO False
  (dec1, dec2) <- concurrently 
    (withLock lock $ runReaderT (copy1 prop1) hvar)
    (withLock lock $ runReaderT (copy2 prop2) hvar)
  return (dec1, dec2)



inspect' :: HiddenVar -> Context Copy -> Context Property -> IO (Context Decision)
inspect' hvar cs ps = do
  lock <- newTVarIO False
  mapConcurrently (\m -> withLock lock $ runReaderT m hvar) (cs <*> ps)


getObs :: Context Copy -> Context Property -> Context (M Decision)
getObs cs ps = cs <*> ps

liftEffect :: Context (M Decision) -> M (Context Decision)
liftEffect = sequence


{--
reifyEffect :: HiddenVar -> M (Context Decision) -> IO (Context Decision)
reifyEffect hvar m = 
--}



runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial {
          source = newTVarIO Nothing
        , copies = return (sys, sys)
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr


{--
runTrial' :: IO ReviewerAgreement
runTrial' = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      tr = Trial' {
          source' = newTVarIO Nothing
        , copies' = return $ Context (sys, sys)
        , reviewers' = Context (r1, r2)
        , measure' = inspect'
      } 
  getAgreement' $ executeTr' tr
--}


main :: IO ()
main = do
  printStats "(Concurrency, Othing)" 12345 runTrial


