module Concur.ConcurrencyTest where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader


-- communication channels 
type ChannelT = TVar (Maybe Bool)
type M = ReaderT ChannelT STM


proc :: Bool -> M Bool
proc mine = do
  channel <- ask
  yours <- lift $ readTVar channel
  case yours of
    Nothing -> do
      lift $ writeTVar channel (Just mine)
      return mine
    Just b -> do
      return b


-- process that overrides with True and False
procTT, procFF :: M Bool
procTT = proc True
procFF = proc False


-- run concurrently
runProc2 :: (M Bool, M Bool) -> IO (Bool, Bool)
runProc2 (p1, p2) = do
  hvar <- newTVarIO Nothing
  (b1, b2) <- concurrently 
    (atomically $ runReaderT p1 hvar)
    (atomically $ runReaderT p2 hvar)
  return (b1, b2)

