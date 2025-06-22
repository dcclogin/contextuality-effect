{-# LANGUAGE MultiParamTypeClasses #-}
module PaperModel where

import Config
import Context2

class (Monad m) => PaperModel m where
  -- src  :: IO s
  -- sys  :: Property -> m Decision
  sys1 :: IO (Property -> m Decision)
  sys2 :: IO (Context (Property -> m Decision))
  run1 :: (Property -> m Decision) -> Context Property -> IO (Context Decision)
  run2 :: Context (Property -> m Decision) -> Context Property -> IO (Context Decision)