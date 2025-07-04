{-# LANGUAGE ConstrainedClassMethods, FlexibleInstances #-}
module PaperCore where

import Config
import RandomUtils
import Cont.EffectT (YieldT)
import Control.Monad.State.Lazy
import Control.Monad (Monad)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader
import Control.Concurrent.STM

-- helper functions for various models

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
  recallDecision :: Property -> (Decision -> Bool) -> m Bool
  recallDecision prop pred = do
    let others = filter (/= prop) [Margins, FontSize, NumPages]
    decisions <- mapM getDecision others
    return (any (\md -> maybe False pred md) decisions)


-- generic for Othing model (State, Concurrency, Continuation)
class (PaperCore m) => PaperOthing m where
  -- get a pixel from the effectul environment
  getPixel :: m (Maybe Pixel)
  -- render an ad hoc decision for 
  -- just one property|predicate|question|attribute|observable
  renderPixel :: (MonadIO m) => Property -> m Pixel
  renderPixel prop = do dec <- renderDecision; return (prop, dec)
  -- render a pixel and put it
  renderPixelP :: (MonadIO m) => Property -> m Pixel
  renderPixelP prop = do dec <- renderDecisionP prop; return (prop, dec)




instance PaperCore (StateT Paper IO) where
  getDecision prop = do
    paper <- get
    case prop of
      Margins  -> return (margins paper)
      FontSize -> return (fontSize paper)
      NumPages -> return (numPages paper)
  putDecision prop d = do
    paper <- get
    case prop of
      Margins  -> put paper { margins = d }
      FontSize -> put paper { fontSize = d }
      NumPages -> put paper { numPages = d }
  renderDecision = liftIO randomDecision


instance PaperCore (ReaderT (TVar Paper) IO) where
  getDecision prop = do
    channel <- ask
    paper <- liftIO $ readTVarIO channel
    case prop of
      Margins   -> return (margins paper)
      FontSize  -> return (fontSize paper)
      NumPages  -> return (numPages paper)
  putDecision prop d = do
    channel <- ask
    paper <- liftIO $ readTVarIO channel
    let newPaper = case prop of
          Margins  -> paper { margins = d }
          FontSize -> paper { fontSize = d }
          NumPages -> paper { numPages = d }
    liftIO $ atomically $ writeTVar channel newPaper
  renderDecision = liftIO randomDecision


instance PaperCore (StateT (Maybe Pixel) IO) where
  getDecision prop = do
    pixel <- get
    return $ case pixel of
      Just (p, dec) | p == prop -> Just dec
      _ -> Nothing
  
  putDecision prop (Just dec) = put (Just (prop, dec))
  putDecision _ Nothing = put Nothing


instance PaperCore (ReaderT (TVar (Maybe Pixel)) IO) where
  putDecision prop (Just dec) = do
    channel <- ask
    liftIO $ atomically $ writeTVar channel (Just (prop, dec))
  putDecision _ Nothing = do
    channel <- ask
    liftIO $ atomically $ writeTVar channel Nothing

instance PaperCore (YieldT Pixel Pixel Decision IO) where



instance PaperOthing (YieldT Pixel Pixel Decision IO) where
instance PaperOthing (StateT (Maybe (Property, Decision)) IO) where
  getPixel = get
instance PaperOthing (ReaderT (TVar (Maybe (Property, Decision))) IO) where
  getPixel = do channel <- ask; liftIO $ readTVarIO channel
instance PaperForget (StateT Paper IO) where
instance PaperForget (ReaderT (TVar Paper) IO) where
instance PaperNothing (StateT Paper IO) where
instance PaperNothing (ReaderT (TVar Paper) IO) where