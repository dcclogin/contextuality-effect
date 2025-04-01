module Prototype02 where

import Control.Monad.Cont
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- Global IORef to hold the suspended continuation.
-- The continuation now expects a String argument.
suspension :: IORef (Maybe (String -> ContT String IO String))
suspension = unsafePerformIO $ newIORef Nothing

-- f performs its first half, suspends waiting for a String,
-- and then, upon being resumed with that String, uses it in its second half.
f :: ContT String IO String
f = do
    liftIO $ putStrLn "f: first half"
    result <- suspend  -- suspend execution and wait for a value
    liftIO $ putStrLn $ "f: second half, received: " ++ result
    return result

-- g does some work and then resumes f with a value.
g :: ContT String IO String
g = do
    liftIO $ putStrLn "g: executing"
    liftIO $ putStrLn "g: resuming f with a value"
    resume "some value from g"

-- suspend captures the current continuation (the rest of f)
-- and stores it in the IORef. The continuation now expects a String.
suspend :: ContT String IO String
suspend = callCC $ \k -> do
    liftIO $ putStrLn "f: suspending"
    liftIO $ writeIORef suspension (Just k)
    -- Return a dummy value; when the continuation is resumed,
    -- the supplied value will replace this.
    return "dummy"

-- resume retrieves the suspended continuation and calls it with the given value.
resume :: String -> ContT String IO String
resume val = do
    maybeK <- liftIO $ readIORef suspension
    case maybeK of
        Just k  -> k val
        Nothing -> error "No suspension available!"

-- runFun controls the overall execution order.
-- It first runs f until its suspension point, then runs g to resume f.
runFun :: ContT String IO String -> ContT String IO String -> IO ()
runFun f g = do
    _ <- runContT f return
    _ <- runContT g return
    return ()

printRun :: IO ()
printRun = runFun f g