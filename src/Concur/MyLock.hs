module Concur.MyLock where

import Control.Concurrent
import Control.Concurrent.STM


type Lock = TVar Bool


acquire :: Lock -> STM ()
acquire lock = do
  taken <- readTVar lock
  if taken then retry else writeTVar lock True


release :: Lock -> STM ()
release = flip writeTVar False


withLock :: Lock -> IO a -> IO a
withLock lock action = do
  atomically $ acquire lock
  result <- action
  atomically $ release lock
  return result


{--
Note: This seems to be a best workaround.

The intial goal is to avoid the situation such as where both reviewers read Nothing.
The read+write should be treated as a bundled atomic action.

[type M = ReaderT ChannelT STM] won't work smoothly due to randomness in semantics.
--}