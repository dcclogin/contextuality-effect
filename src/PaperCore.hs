{-# LANGUAGE ConstrainedClassMethods #-}
module PaperCore where

import Config
import RandomUtils
import Control.Monad (Monad)
import Control.Monad.IO.Class (MonadIO, liftIO)


class (Monad m) => PaperCore m where
  -- get a decision of a property in the effectful environment
  getDecision :: Property -> m (Maybe Decision)
  -- put a decision of a property to the effectful environment
  putDecision :: Property -> Maybe Decision -> m ()
  -- render a decision within the effectful environment
  -- by default using randomDecision
  renderDecision :: (MonadIO m) => m Decision
  renderDecision = liftIO randomDecision
  -- render a decision of a property and put it to the effectful environment
  renderDecisionP :: (MonadIO m) => Property -> m Decision
  renderDecisionP prop = do
    dec <- renderDecision
    putDecision prop (Just dec)
    return dec


-- generic for Forget model (State, Concurrency)
class (PaperCore m) => PaperForget m where
  -- forget a decision of a property unconditionally
  forgetDecision :: Property -> m ()
  forgetDecision prop = putDecision prop Nothing
  -- forget a decision of a property under a condition
  cforgetDecision :: Property -> (Decision -> Bool) -> m ()
  cforgetDecision prop pred = do
    d <- getDecision prop
    case d of
      Just dec | pred dec -> forgetDecision prop
      _ -> return ()
  -- forgetful getDecision    
  getDecisionF :: Property -> m (Maybe Decision)
  getDecisionF prop = do
    d <- getDecision prop
    case d of
      Nothing -> return Nothing
      Just dec -> do
        let others = filter (/= prop) [Margins, FontSize, NumPages]
        mapM_ (\p -> cforgetDecision p (== dec)) others
        return d


-- generic for Nothing model (State, Concurrency, Continuation)
class (PaperCore m) => PaperNothing m where
  -- check if any other properties have made a specific decision 
  crecallDecision :: Property -> (Decision -> Bool) -> m Bool
  crecallDecision prop pred = do
    let others = filter (/= prop) [Margins, FontSize, NumPages]
    decisions <- mapM getDecision others
    return (any (\md -> maybe False pred md) decisions)


-- generic for Othing model (State, Concurrency, Continuation)
class (PaperCore m) => PaperOthing m where
  -- get a pixel from the effectul environment
  getPixel :: m (Maybe (Property, Decision))
  -- render an ad hoc decision for 
  -- just one property|predicate|question|attribute|observable
  renderPixel :: (MonadIO m) => Property -> m (Property, Decision)
  renderPixel prop = do dec <- renderDecision; return (prop, dec)
  -- render a pixel and put it
  renderPixelP :: (MonadIO m) => Property -> m (Property, Decision)
  renderPixelP prop = do dec <- renderDecisionP prop; return (prop, dec)