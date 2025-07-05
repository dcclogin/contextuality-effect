{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module ExperimentRunner where

import MeasurementScenario

--------------------------------------------------------------------------------
-- Abstract Contextual Interface
--------------------------------------------------------------------------------

-- | A monad in which observables can be assigned values
class Applicative m => Contextual m o v | m -> o v where
  measure :: o -> m v

--------------------------------------------------------------------------------
-- Core Logic: Context Evaluation
--------------------------------------------------------------------------------

-- | Run a single context by measuring each observable and applying its constraint
runContext :: (Monad m, Contextual m o v) => ContextDef o v -> m Bool
runContext (ContextDef obs constraint _) = do
  vs <- traverse measure obs
  pure (constraint vs)

-- | Run all contexts in an experiment, returning their satisfaction status
runExperiment :: (Monad m, Contextual m o v) => ExperimentDef o v -> m [([o], Bool)]
runExperiment (ExperimentDef _ cs) =
  traverse (\ctx -> (\ok -> (contextObservables ctx, ok)) <$> runContext ctx) cs

-- | Return True if all contexts are satisfied
experimentSucceeds :: (Monad m, Contextual m o v) => ExperimentDef o v -> m Bool
experimentSucceeds expDef = all snd <$> runExperiment expDef

-- | Print a single context's result
printResult :: Show o => ([o], Bool) -> IO ()
printResult (obs, ok) = putStrLn $ show obs ++ " ⇒ " ++ show ok

