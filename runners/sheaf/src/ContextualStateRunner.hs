{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module ContextualStateRunner where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import MeasurementScenario
import ExperimentRunner

--------------------------------------------------------------------------------
-- Generic state monad over assignments
--------------------------------------------------------------------------------

type Assignments o v = Map o v

-- | Domain of possible values for each observable (can be customized)
defaultDomain :: Num v => [v]
defaultDomain = [1, -1]

-- | State monad that stores:
--   - Partial assignment: Map o v
--   - Current context observables: [o]
--   - Constraint to satisfy: [v] -> Bool
newtype LocalState o v a = LocalState
  { runLocalState :: State (Assignments o v, [o], Constraint v) a }
  deriving (Functor, Applicative, Monad)

-- | Attempt to assign a value to an observable consistent with the current constraint
instance (Show o, Ord o, Eq v, Num v) => Contextual (LocalState o v) o v where
  measure o = LocalState $ do
    (env, ctx, constraint) <- get
    case Map.lookup o env of
      Just v  -> return v
      Nothing -> do
        let unassigned = filter (`Map.notMember` env) ctx
            isLast     = length unassigned == 1

            -- Try assigning a value that satisfies the constraint if last
            satisfiesConstraint v =
              let env' = Map.insert o v env
                  vs   = [env' Map.! x | x <- ctx]
              in not isLast || constraint vs

            viable = filter satisfiesConstraint defaultDomain

        case viable of
          (v:_) -> do
            put (Map.insert o v env, ctx, constraint)
            return v
          [] -> error $ "No valid value for observable " ++ show o

--------------------------------------------------------------------------------
-- Run experiment using this local state assignment monad
--------------------------------------------------------------------------------

runWithContextualState :: (Ord o, Show o, Eq v, Num v, Show v) => ExperimentDef o v -> IO ()
runWithContextualState expDef = do
  let results =
        [ let ok = evalState (runLocalState (runContext ctx))
                             (Map.empty, contextObservables ctx, contextConstraint ctx)
          in (contextObservables ctx, ok)
        | ctx <- contexts expDef
        ]
  mapM_ printResult results
  putStrLn $ "Experiment succeeds: " ++ show (all snd results)

