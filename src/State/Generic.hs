{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module State.Generic where

import Config
import Context2
import Contextuality
import Control.Monad.State.Lazy


instance Contextuality Context (StateT s IO) s where
  run1S s c ps   = evalStateT (traverse c ps) s
  run2S s cs ps  = evalStateT (entangle $ cs <*> ps) s
  run2AS s cs ps = traverse (\m -> evalStateT m s) (cs <*> ps)