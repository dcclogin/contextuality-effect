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
  -- render a random decision of a property and put it to the environment
  renderDecision :: (MonadIO m) => Property -> m Decision
  renderDecision prop = do
    dd <- liftIO randomDecision
    putDecision prop (Just dd)
    return dd


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


class (PaperCore m) => PaperNothing m where
  -- check if any other properties have made a specific decision 
  crecallDecision :: Property -> (Decision -> Bool) -> m Bool
  crecallDecision prop pred = do
    let others = filter (/= prop) [Margins, FontSize, NumPages]
    decisions <- mapM getDecision others
    return (any (\md -> maybe False pred md) decisions)