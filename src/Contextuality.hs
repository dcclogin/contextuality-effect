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
import Concur.AtomicIO (atomicIO)


-- f : the applicative context, commuting operations
-- m : the monadic computational effect
-- s : source that holds hidden variables
-- m -> s : every m comes with a unique s
class (Applicative f, Traversable f, Monad m) => Contextuality f m s | m -> s, m -> f where

  -- (optional) reify effect for a single monad
  reify1 :: s -> m Decision -> IO Decision

  -- (optional) reify effect for all monads within the context f
  reifyf :: s -> m (f Decision) -> IO (f Decision)

  -- (optional) traverse the context f in various manners
  traverseC :: (m Decision -> IO Decision) -> f (m Decision) -> IO (f Decision)

  -- (optional) run and get the result inside effectful environment m
  runfM :: f (Copy m) -> f Property -> m (f Decision)

  -- run All and get the result in IO environment
  runfA :: s -> f (Copy m) -> f Property -> IO (f Decision)
  runfA s cs ps = reifyf s (runfM cs ps)

  -- (optional) run Parallel and get the result in IO environment
  runfP :: s -> f (Copy m) -> f Property -> IO (f Decision)
  runfP s cs ps = traverseC (reify1 s) (cs <*> ps)

  -- [ TODO ] rename: runEntangled, runSeparated
  -- [ TODO ] add: runPartial*


-- generic instance for State (Forget, Nothing, Othing)
instance Contextuality Context (StateT s IO) s where 
  reify1 = flip evalStateT
  reifyf = flip evalStateT
  traverseC = traverse
  runfM cs ps = sequence $ cs <*> ps

  -- runfA s cs ps = evalStateT (sequence $ cs <*> ps) s
  -- runfP s cs ps = traverse (\m -> evalStateT m s) (cs <*> ps)

-- generic instance for Concurrency (Forget, Nothing, Othing)
instance Contextuality Context (ReaderT (TVar s) IO) (TVar s) where
  reify1 s m = atomicIO $ runReaderT m s
  reifyf = flip runReaderT
  traverseC = mapConcurrently
  runfM cs ps = sequence $ cs <*> ps
  
  -- runfA s cs ps = runReaderT (sequence $ cs <*> ps) s
  -- runfP s cs ps = mapConcurrently (\m -> atomicIO $ runReaderT m s) (cs <*> ps)


-- generic instance for Continuation (Forget, Nothing, Othing)