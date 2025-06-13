module Concur.MyLock where

import Control.Concurrent
import Control.Concurrent.STM


type Lock = TVar Bool


acquire :: Lock -> STM ()
acquire lock = do
  taken <- readTVar lock
  if taken
    then retry
    else writeTVar lock True


release :: Lock -> STM ()
release = flip writeTVar False


withLock :: Lock -> IO a -> IO a
withLock lock action = do
  atomically $ acquire lock
  result <- action
  atomically $ release lock
  return result