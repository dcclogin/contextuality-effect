{-# LANGUAGE FlexibleContexts #-}
module Prototype00 where

import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- Our communication channel is an IORef that holds an exit continuation.
type Channel = IORef (Bool -> ContT Bool IO Bool)

-- f now takes an extra argument for communication (the channel)
-- as well as an explicit exit continuation passed from runFun.
f :: Channel -> (Bool -> ContT Bool IO Bool) -> ContT Bool IO Bool
f ch exit = do
    liftIO $ putStrLn "f: Starting execution."
    -- Store the exit continuation into the channel.
    liftIO $ writeIORef ch exit
    liftIO $ putStrLn "f: Waiting for future result..."
    -- Return a dummy value (which will be discarded if the exit continuation is invoked).
    return False

-- g also takes the extra channel and the shared exit continuation.
g :: Channel -> (Bool -> ContT Bool IO Bool) -> ContT Bool IO Bool
g ch _exit = do
    liftIO $ putStrLn "g: Executing."
    -- Retrieve the exit continuation from the channel.
    exit <- liftIO $ readIORef ch
    liftIO $ putStrLn "g: Triggering f's continuation with its result."
    -- Invoke the exit continuation with the desired result.
    exit True

-- runFun wires together f and g.
-- We use an outer callCC to capture an exit continuation that covers both.
runFun :: (Channel -> (Bool -> ContT Bool IO Bool) -> ContT Bool IO Bool)
       -> (Channel -> (Bool -> ContT Bool IO Bool) -> ContT Bool IO Bool)
       -> IO Bool
runFun funF funG = runContT (callCC $ \exit -> do
    -- Create a channel holding the shared exit continuation.
    ch <- liftIO $ newIORef exit
    -- Run f first, then g. If g calls the exit continuation,
    -- it aborts the remaining computation and supplies the final result.
    _ <- funF ch exit
    funG ch exit
  ) return

printRun :: IO ()
printRun = do
    finalResult <- runFun f g
    putStrLn $ "Final result: " ++ show finalResult