module Concur.Effect where

import Config (Paper)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad.Reader

-- communication channels for two observables
type ChannelT = TVar Paper
type M = ReaderT ChannelT STM