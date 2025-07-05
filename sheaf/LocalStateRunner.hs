{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LocalStateRunner (runWithLocalState) where

import Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)
import MeasurementScenario
import ExperimentRunner

--------------------------------------------------------------------------------
-- Generic state monad over assignments
--------------------------------------------------------------------------------

type Assignments o v = Map o v

-- We parameterize over observable type `o` and value type `v`
newtype LocalState o v a = LocalState { runLocalState :: State (Assignments o v, [o], [v] -> Bool) a }
  deriving (Functor, Applicative, Monad)

-- In this generic form, we require Eq, Ord, and Show for observables and values
instance (Ord o, Eq v, Num v) => Contextual (LocalState o v) o v where
  measure o = LocalState $ do
    (env, ctx, constraint) <- get
    case Map.lookup o env of
      Just v -> return v
      Nothing -> do
        let unassigned = filter (`Map.notMember` env) ctx
        let isLast = length unassigned == 1
        let candidates = [1, -1]  -- or more general domain later
        let tryValue v =
              let env' = Map.insert o v env
              in if isLast
                   then constraint [env' Map.! x | x <- ctx]
                   else True  -- any value is OK for now
        let v = case filter tryValue candidates of
                  (v1:_) -> v1
                  []     -> 1  -- fallback
        put (Map.insert o v env, ctx, constraint)
        return v
 
--------------------------------------------------------------------------------
-- Generic experiment runner using this monad
--------------------------------------------------------------------------------

runWithLocalState :: (Ord o, Show o, Eq v, Num v, Show v) => ExperimentDef o v -> IO ()
runWithLocalState expDef = do
  let results =
        [ let ok = evalState (runLocalState (runContext ctx))
                             (Map.empty, contextObservables ctx, contextConstraint ctx)
          in (contextObservables ctx, ok)
        | ctx <- contexts expDef
        ]
  mapM_ printResult results
  putStrLn $ "Experiment succeeds: " ++ show (all snd results)

printResult :: (Show o) => ([o], Bool) -> IO ()
printResult (obs, ok) = putStrLn $ show obs ++ " ⇒ " ++ show ok