{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module ExperimentRunner where

import Control.Applicative
import Control.Monad.Identity

import MeasurementScenario
import FamousScenarios

-- | Abstract interface: any monad that can 'measure' an observable to produce a value
class Applicative m => Contextual m o v | m -> o v where
  measure :: o -> m v

-- | Run a single context: measure observables, apply constraint
runContext :: (Monad m, Contextual m o v) => ContextDef o v -> m Bool
runContext (ContextDef obs constraint) = do
  vs <- sequenceA (map measure obs)
  pure (constraint vs)

-- | Run all contexts in an experiment and return their results
runExperiment :: (Monad m, Contextual m o v) => ExperimentDef o v -> m [( [o], Bool )]
runExperiment (ExperimentDef _ ctxts) =
  traverse (\ctx -> (\ok -> (contextObservables ctx, ok)) <$> runContext ctx) ctxts

-- | True if all contexts satisfy their constraints
experimentSucceeds :: (Monad m, Contextual m o v) => ExperimentDef o v -> m Bool
experimentSucceeds expDef = all snd <$> runExperiment expDef

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- A dummy global assignment — same value for all observables

instance Contextual Identity Observable Outcome where
  measure o = pure (assignment o)

assignment :: Observable -> Outcome
assignment "Ax" = 1
assignment "Bx" = 1
assignment "Cx" = 1
assignment _    = 1

testGHZ :: Bool
testGHZ = runIdentity (experimentSucceeds ghz)  -- Expect: False

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------
