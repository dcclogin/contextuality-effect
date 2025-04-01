{-# LANGUAGE FlexibleContexts #-}

module Prototype1 where

import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- The channel holds an optional pair: f's original value and its captured continuation.
type Channel = IORef (Maybe (Bool, ((Bool, Bool) -> ContT (Bool, Bool) IO (Bool, Bool))))

-- f computes an initial value and then uses callCC to “pause” its computation,
-- storing its original value and continuation in the channel.
f :: Channel -> ContT (Bool, Bool) IO (Bool, Bool)
f ch = callCC $ \exitF -> do
    liftIO $ putStrLn "f: Starting execution."
    let fVal = False  -- f's original value
    -- Store f's original value along with its captured continuation.
    liftIO $ writeIORef ch (Just (fVal, exitF))
    liftIO $ putStrLn "f: Waiting for correction from g..."
    -- Fallback value: if g never intervenes, f would return (fVal, fVal).
    return (fVal, fVal)

-- g retrieves the stored value and continuation from the channel,
-- computes its own result (the correction), and then triggers f's continuation.
g :: Channel -> ContT (Bool, Bool) IO (Bool, Bool)
g ch = do
    liftIO $ putStrLn "g: Executing."
    mPair <- liftIO $ readIORef ch
    case mPair of
      Nothing -> error "g: No value found in channel"
      Just (fVal, exitF) -> do
          let gVal = True  -- g's computed (correcting) value
          liftIO $ putStrLn "g: Correcting f's value."
          -- Calling exitF short-circuits f's computation and supplies the final pair.
          exitF (fVal, gVal)
          -- The following line is never reached.
          return (fVal, gVal)

-- runFun creates the channel and runs f and g sequentially.
-- Because of the callCC in f, when g calls the stored continuation,
-- the overall computation returns the pair (f's original value, g's correction).
runFun :: (Channel -> ContT (Bool, Bool) IO (Bool, Bool))
       -> (Channel -> ContT (Bool, Bool) IO (Bool, Bool))
       -> IO (Bool, Bool)
runFun funF funG = do
    ch <- newIORef Nothing
    runContT (funF ch >> funG ch) return

printRun :: IO ()
printRun = do
    (fResult, gResult) <- runFun f g
    putStrLn $ "Final f result (original): " ++ show fResult
    putStrLn $ "Final g result (correction): " ++ show gResult