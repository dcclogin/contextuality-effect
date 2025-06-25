{-# LANGUAGE FunctionalDependencies #-}
module Contextuality where

import Config
import Context2

-- s : source that holds hidden variables
-- m : the monadic computational effect
-- m -> s : every m comes with a unique s
class (Monad m) => Contextuality s m | m -> s where
  -- exposing source
  run1S  :: s -> Copy m -> Context Property -> IO (Context Decision)
  run2S  :: s -> Context (Copy m) -> Context Property -> IO (Context Decision)
  run2AS :: s -> Context (Copy m) -> Context Property -> IO (Context Decision)
  -- hidding source to reviewers
  run1   :: Copy m -> Context Property -> IO (Context Decision)
  run2   :: Context (Copy m) -> Context Property -> IO (Context Decision)
  run2A  :: Context (Copy m) -> Context Property -> IO (Context Decision)