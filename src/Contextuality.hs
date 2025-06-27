{-# LANGUAGE FunctionalDependencies #-}
module Contextuality where

import Config (Property, Decision, Copy)


-- f : the applicative context, commuting operations
-- m : the monadic computational effect
-- s : source that holds hidden variables
-- m -> s : every m comes with a unique s
class (Applicative f, Traversable f, Monad m) => Contextuality f m s | m -> s where
  -- exposing source
  run1S  :: s -> Copy m -> f Property -> IO (f Decision)
  run2S  :: s -> f (Copy m) -> f Property -> IO (f Decision)
  run2AS :: s -> f (Copy m) -> f Property -> IO (f Decision)
  -- hidding source to reviewers
  run1   :: Copy m -> f Property -> IO (f Decision)
  run2   :: f (Copy m) -> f Property -> IO (f Decision)
  run2A  :: f (Copy m) -> f Property -> IO (f Decision)
  -- rename suggested by ChatGPT
  -- runEntangled, runSeparated