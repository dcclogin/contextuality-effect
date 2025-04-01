module Prototype01 where

import Control.Monad.Cont
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- A global IORef to hold the suspended continuation.
suspension :: IORef (Maybe ( () -> ContT () IO () ))
suspension = unsafePerformIO $ newIORef Nothing

-- f does its first half and then suspends its execution.
f :: ContT () IO ()
f = do
  liftIO $ putStrLn "f: first half"
  suspend  -- suspend execution here (f doesn’t mention g)
  liftIO $ putStrLn "f: second half"

-- g is defined separately. It does some work and then resumes f.
g :: ContT () IO ()
g = do
  liftIO $ putStrLn "g: executing"
  resume   -- resume the suspended part of f

-- suspend captures the current continuation (the remainder of f)
-- and stores it for later use.
suspend :: ContT () IO ()
suspend = callCC $ \k -> do
  liftIO $ putStrLn "f: suspending"
  liftIO $ writeIORef suspension (Just k)
  return ()

-- resume retrieves the suspended continuation and calls it.
resume :: ContT () IO ()
resume = do
  mk <- liftIO $ readIORef suspension
  case mk of
    Just k -> do
      liftIO $ putStrLn "g: resuming f"
      k ()
    Nothing -> error "No suspension available!"

-- runFun controls the execution order.
-- It runs f until its suspension point, then runs g, which resumes f.
runFun :: ContT () IO () -> ContT () IO () -> IO ()
runFun f g = do
  runContT f (\_ -> return ())
  runContT g (\_ -> return ())

printRun :: IO ()
printRun = runFun f g