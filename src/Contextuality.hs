{-# LANGUAGE 
    FunctionalDependencies
  , AllowAmbiguousTypes
  , FlexibleInstances
  , MultiParamTypeClasses 
#-}
module Contextuality where

import Config (Property, Decision, Copy)
import Context2
import Control.Monad.State.Lazy
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader
import Concur.MyLock (atomicIO)


-- f : the applicative context, commuting operations
-- m : the monadic computational effect
-- s : source that holds hidden variables
-- m -> s : every m comes with a unique s
class (Applicative f, Traversable f, Monad m) => Contextuality f m s | m -> s where
  -- src    :: IO s
  -- exposing source
  run1S  :: s -> Copy m -> f Property -> IO (f Decision)
  run2S  :: s -> f (Copy m) -> f Property -> IO (f Decision)
  run2AS :: s -> f (Copy m) -> f Property -> IO (f Decision)
  -- [ TODO ] rename: runEntangled, runSeparated
  -- [ TODO ] add: runPartial*


-- generic instance for State (Forget, Nothing, Othing)
instance Contextuality Context (StateT s IO) s where 
  run1S s c ps   = evalStateT (traverse c ps) s
  run2S s cs ps  = evalStateT (sequence $ cs <*> ps) s
  run2AS s cs ps = traverse (\m -> evalStateT m s) (cs <*> ps)


-- generic instance for Concurrency (Forget, Nothing, Othing)
instance Contextuality Context (ReaderT (TVar s) IO) (TVar s) where
  run1S s c ps = 
    mapConcurrently (\m -> atomicIO $ runReaderT m s) (fmap c ps)
  run2S s cs ps = 
    runReaderT (sequence $ cs <*> ps) s
  run2AS s cs ps = 
    mapConcurrently (\m -> atomicIO $ runReaderT m s) (cs <*> ps)