{-# LANGUAGE FlexibleContexts #-}
module Prototype0 where

import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- The channel is simply an IORef that will hold f's captured continuation.
type Channel = IORef (Bool -> ContT Bool IO Bool)

-- f captures its current continuation using callCC and stores it in the channel.
f :: Channel -> ContT Bool IO Bool
f ch = callCC $ \exit -> do
    liftIO $ putStrLn "f: Starting execution."
    -- Store the captured continuation in the channel.
    liftIO $ writeIORef ch exit
    liftIO $ putStrLn "f: Waiting for future result..."
    -- This fallback will not be used, because g will invoke the continuation.
    return False

-- g retrieves the continuation from the channel and uses it to supply a result.
g :: Channel -> ContT Bool IO Bool
g ch = do
    liftIO $ putStrLn "g: Executing."
    exit <- liftIO $ readIORef ch
    let result = True  -- Compute a Bool result.
    liftIO $ putStrLn "g: Triggering f's continuation with its result."
    exit result

-- runFun creates the communication channel and runs f and g sequentially.
runFun :: (Channel -> ContT Bool IO Bool)
       -> (Channel -> ContT Bool IO Bool)
       -> IO Bool
runFun funF funG = do
    -- Create a channel to hold the continuation.
    ch <- newIORef (error "exit continuation not set")
    -- Run funF followed by funG in the continuation monad.
    runContT (funF ch >> funG ch) return

printRun :: IO ()
printRun = do
    finalResult <- runFun f g
    putStrLn $ "Final result: " ++ show finalResult