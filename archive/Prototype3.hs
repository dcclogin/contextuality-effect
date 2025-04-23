{-# LANGUAGE FlexibleContexts #-}
module Prototype3 where

import Control.Monad.Cont
import Control.Monad.IO.Class (liftIO)
import Data.IORef

-- A slot stores the initial value and the captured continuation.
data Slot = Slot
  { initVal :: Bool
  , contVal :: (Bool, Bool) -> ContT (Bool, Bool) IO (Bool, Bool)
  }

-- The channel holds at most one slot along with an identifier for which function stored it.
type Channel = IORef (Maybe (String, Slot))

-- A simple combination function that “adjusts” the two initial values.
combine :: Bool -> Bool -> (Bool, Bool)
combine fVal gVal = (fVal && gVal, fVal || gVal)

-- Function f is written symmetrically: it computes an initial value, captures its continuation,
-- and then either stores its slot if no other value is present or combines its value with g’s.
f :: Channel -> ContT (Bool, Bool) IO (Bool, Bool)
f ch = callCC $ \exitF -> do
    liftIO $ putStrLn "f: Starting execution."
    let fInit = False
    let slotF = Slot fInit exitF
    mSlot <- liftIO $ readIORef ch
    case mSlot of
      Nothing -> do
          liftIO $ putStrLn "f: No partner yet, storing my slot and waiting..."
          liftIO $ writeIORef ch (Just ("f", slotF))
          -- Return a fallback (should never be used because the other function will complete the computation)
          return (fInit, fInit)
      Just (tag, otherSlot) ->
          if tag == "g" then do
              let gInit = initVal otherSlot
              let finalPair = combine fInit gInit
              liftIO $ putStrLn $ "f: Found g's slot. Combining values: " ++ show finalPair
              -- Complete g's waiting call by invoking its continuation
              _ <- contVal otherSlot finalPair
              -- And complete f by exiting with the final pair.
              exitF finalPair
          else
              error "f: Unexpected duplicate entry in channel."

-- Function g is symmetric to f.
g :: Channel -> ContT (Bool, Bool) IO (Bool, Bool)
g ch = callCC $ \exitG -> do
    liftIO $ putStrLn "g: Starting execution."
    let gInit = True
    let slotG = Slot gInit exitG
    mSlot <- liftIO $ readIORef ch
    case mSlot of
      Nothing -> do
          liftIO $ putStrLn "g: No partner yet, storing my slot and waiting..."
          liftIO $ writeIORef ch (Just ("g", slotG))
          return (gInit, gInit)
      Just (tag, otherSlot) ->
          if tag == "f" then do
              let fInit = initVal otherSlot
              let finalPair = combine fInit gInit
              liftIO $ putStrLn $ "g: Found f's slot. Combining values: " ++ show finalPair
              _ <- contVal otherSlot finalPair
              exitG finalPair
          else
              error "g: Unexpected duplicate entry in channel."

-- runFun creates the shared channel and runs f and g sequentially.
-- (The order does not matter: whichever runs first will store its slot and wait for the other.)
runFun :: (Channel -> ContT (Bool, Bool) IO (Bool, Bool))
       -> (Channel -> ContT (Bool, Bool) IO (Bool, Bool))
       -> IO (Bool, Bool)
runFun funF funG = do
    ch <- newIORef Nothing
    -- Run the two functions sequentially.
    -- One of them will store its slot and wait, and the other will complete the computation.
    _ <- runContT (funF ch) return
    runContT (funG ch) return

printRun :: IO ()
printRun = do
    (finalF, finalG) <- runFun f g
    putStrLn $ "Final results: f = " ++ show finalF ++ ", g = " ++ show finalG