module Concur.Effect where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async

-- communication channels for two observables
type ChannelM = MVar (Bool, Int)
type ChannelT = TVar (Bool, Int)