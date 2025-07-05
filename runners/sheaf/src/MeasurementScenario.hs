module MeasurementScenario where

import Data.List (nub)

-- | An observable represents a single measurement, e.g., "Ax", "By", etc.
type Observable = String

-- | Outcomes (e.g., ±1 for dichotomic measurements)
type Outcome = Int

-- | A constraint over the joint outcomes of a context (e.g., product = 1)
type Constraint v = [v] -> Bool

-- | A single measurement context:
--   - A list of compatible observables
--   - A constraint on their joint outcomes
data ContextDef o v = ContextDef
  { contextObservables :: [o]           -- joint measurements
  , contextConstraint  :: Constraint v  -- expected relationship
  , contextWeight      :: Maybe ([v] -> Double)
  } 

-- | A full experiment consists of multiple contexts and a set of all observables
data ExperimentDef o v = ExperimentDef
  { allObservables :: [o]
  , contexts       :: [ContextDef o v]
  } 

--------------------------------------------------------------------------------
-- DSL-style builders
--------------------------------------------------------------------------------

-- | Build a context from observables and a constraint
context :: [o] -> Constraint v -> ContextDef o v
context observables constraints = ContextDef observables constraints Nothing

-- | Build a context with a full probabilistic weight function
weightedContext :: [o] -> Constraint v -> ([v] -> Double) -> ContextDef o v
weightedContext observables constraints weight =
  ContextDef observables constraints (Just weight)

-- | Build a context with a binary accept/reject predicate
--   This is useful for Hardy, KCBS, etc.
probabilisticContext :: [o] -> Constraint v -> ([v] -> Bool) -> ContextDef o v
probabilisticContext observables constraints accept =
  weightedContext observables constraints (\vs -> if accept vs then 1.0 else 0.0)

-- | Build a full experiment from a list of contexts
experiment :: Eq o => [ContextDef o v] -> ExperimentDef o v
experiment cs = ExperimentDef
  { allObservables = nub (concatMap contextObservables cs)
  , contexts = cs
  }

--------------------------------------------------------------------------------
-- Common constraints
--------------------------------------------------------------------------------

-- | Product of outcomes equals expected value
parityConstraint :: (Eq v, Num v) => v -> Constraint v
parityConstraint expected vs = product vs == expected

-- | Sum of outcomes equals expected value
sumConstraint :: (Eq v, Num v) => v -> Constraint v
sumConstraint expected vs = sum vs == expected

-- | Equality constraint: all outcomes equal the same value
allEqualConstraint :: Eq v => Constraint v
allEqualConstraint []     = True
allEqualConstraint (x:xs) = all (== x) xs