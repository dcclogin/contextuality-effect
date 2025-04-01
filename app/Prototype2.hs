{-# LANGUAGE RecursiveDo #-}
module Prototype2 where

import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)

-- f takes as argument the future result from g.
f :: Bool -> ContT (Bool, Bool) IO Bool
f gFuture = callCC $ \exitF -> do
    liftIO $ putStrLn "f: Starting execution."
    let fInit = False  -- f’s initial computed value
    liftIO $ putStrLn $ "f: Adjusting value using future g value: " ++ show gFuture
    let finalF = fInit && gFuture   -- f’s final value depends on g’s outcome
    exitF finalF

-- g takes as argument the future result from f.
g :: Bool -> ContT (Bool, Bool) IO Bool
g fFuture = callCC $ \exitG -> do
    liftIO $ putStrLn "g: Starting execution."
    let gInit = True  -- g’s initial computed value
    liftIO $ putStrLn $ "g: Adjusting value using future f value: " ++ show fFuture
    let finalG = gInit || fFuture   -- g’s final value depends on f’s outcome
    exitG finalG

-- runFun uses mdo (recursive do) to tie the knot.
-- f and g are called “simultaneously” in the sense that each
-- receives the future outcome of the other.
runFun :: ContT (Bool, Bool) IO (Bool, Bool)
runFun = mdo
    fVal <- f gVal
    gVal <- g fVal
    return (fVal, gVal)

printRun :: IO ()
printRun = do
    (fRes, gRes) <- runContT runFun return
    putStrLn $ "Final f result: " ++ show fRes
    putStrLn $ "Final g result: " ++ show gRes