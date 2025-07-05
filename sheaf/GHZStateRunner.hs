{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module GHZStateRunner where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import MeasurementScenario
import FamousScenarios
import ExperimentRunner

--------------------------------------------------------------------------------
-- Context-dependent assignments using State
--------------------------------------------------------------------------------

type Assignments = Map Observable Outcome

newtype LocalState a = LocalState { runLocalState :: State (Assignments, [Observable]) a }
  deriving (Functor, Applicative, Monad)

instance Contextual LocalState Observable Outcome where
  measure o = LocalState $ do
    (env, ctx) <- get
    case Map.lookup o env of
      Just v -> return v
      Nothing -> do
        let assigned = Map.size env
        case assigned of
          0 -> do
            let v = 1
            put (Map.insert o v env, ctx)
            return v
          1 -> do
            let v = 1
            put (Map.insert o v env, ctx)
            return v
          2 -> do
            let [a,b,c] = ctx
                Just v1 = Map.lookup a env
                Just v2 = Map.lookup b env
                -- Compute third to satisfy the constraint:
                -- parity = -1 iff ["Ax","Bx","Cx"]
                parity = if ctx == ["Ax","Bx","Cx"] then -1 else 1
                v = parity * v1 * v2
            put (Map.insert o v env, ctx)
            return v
          _ -> error "Too many assignments"

--------------------------------------------------------------------------------
-- Run each context with its own fresh state
--------------------------------------------------------------------------------

printResult :: ([Observable], Bool) -> IO ()
printResult (obs, ok) = putStrLn $ show obs ++ " ⇒ " ++ show ok

runGHZWithState :: IO ()
runGHZWithState = do
  let results =
        [ let ok = evalState (runLocalState (runContext ctx :: LocalState Bool))
                             (Map.empty, contextObservables ctx)
          in (contextObservables ctx, ok)
        | ctx <- contexts ghz
        ]
  mapM_ printResult results
  putStrLn $ "Experiment succeeds: " ++ show (all snd results)
  