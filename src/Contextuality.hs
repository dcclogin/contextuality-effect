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
import Cont.EffectT


-- f : the applicative context, commuting operations
-- m : the monadic computational effect
-- s : source that holds hidden variables
-- m -> s : every m comes with a unique s
class (Applicative f, Traversable f, Monad m) => Contextuality f m s | m -> s, m -> f where
  -- (optional) reify effect for a single monad
  reifySingle :: s -> m Decision -> IO Decision
  -- (optional) reify effect for all monads within the context f
  reifyForall :: s -> m (f Decision) -> IO (f Decision)
  -- (optional) traverse the context f in various manners
  traverseCtx :: (m Decision -> IO Decision) -> f (m Decision) -> IO (f Decision)
  -- measurement of all observables in the context f
  measure :: f (Copy m) -> f Property -> f (m Decision)
  measure = (<*>)
  -- joint measurement of all observables in the context f
  jointM :: f (m Decision) -> m (f Decision)
  jointM = sequence
  -- run all sequentially and get the result in IO environment
  runfSeq :: s -> f (Copy m) -> f Property -> IO (f Decision)
  runfSeq s cs ps = reifyForall s (jointM $ measure cs ps)
  -- (optional) run all in parallel and get the result in IO environment
  runfPar :: s -> f (Copy m) -> f Property -> IO (f Decision)
  runfPar s cs ps = traverseCtx (reifySingle s) (measure cs ps)
  -- placeholder for other run methods
  runfAll :: s -> f (Copy m) -> f Property -> IO (f Decision)


-- generic instance for State (Forget, Nothing, Othing)
instance Contextuality Context (StateT s IO) s where 
  reifySingle = flip evalStateT
  reifyForall = flip evalStateT
  traverseCtx = traverse
  -- runfSeq s cs ps = evalStateT (sequence $ cs <*> ps) s
  -- runfPar s cs ps = traverse (\m -> evalStateT m s) (cs <*> ps)


-- generic instance for Concurrency (Forget, Nothing, Othing)
instance Contextuality Context (ReaderT (TVar s) IO) (TVar s) where
  reifySingle s m = atomicIO $ runReaderT m s
  reifyForall = flip runReaderT
  traverseCtx = mapConcurrently
  -- runfSeq s cs ps = runReaderT (sequence $ cs <*> ps) s
  -- runfPar s cs ps = mapConcurrently (\m -> atomicIO $ runReaderT m s) (cs <*> ps)


-- generic instance for Continuation (Nothing, Othing)
instance Contextuality Context (YieldT i o Decision IO) (Judge Context i o) where
  runfAll (Judge mediate) cs ps = do
    suspended <- traverse runYieldT (measure cs ps)
    mediate suspended