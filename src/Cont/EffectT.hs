module Cont.EffectT where

import Control.Monad.Cont

-- Iterator with monadic continuation
data IteratorT i o r m =
    Result r
  | Susp o (i -> m (IteratorT i o r m))

-- Yield monad transformer
newtype YieldT i o r m a = YieldT { unY :: ContT (IteratorT i o r m) m a }

instance Functor m => Functor (YieldT i o r m) where
  fmap f (YieldT m) = YieldT (fmap f m)

instance Applicative m => Applicative (YieldT i o r m) where
  pure a = YieldT (pure a)
  YieldT mf <*> YieldT ma = YieldT (mf <*> ma)

instance Monad m => Monad (YieldT i o r m) where
  YieldT m >>= f = YieldT (m >>= \a -> unY (f a))

instance MonadTrans (YieldT i o r) where
  lift ma = YieldT (lift ma)

instance MonadIO m => MonadIO (YieldT i o r m) where
  liftIO = lift . liftIO

instance Monad m => MonadCont (YieldT i o r m) where
  callCC f = YieldT $ callCC $ \k ->
    unY $ f (\a -> YieldT (k a))

runYieldT :: Monad m => YieldT i o r m r -> m (IteratorT i o r m)
runYieldT (YieldT m) = runContT m (return . Result)

yield :: Monad m => o -> YieldT i o r m i
yield o = callCC $ \k ->
  YieldT $ ContT $ \_ -> return $ Susp o (\i -> runYieldT (k i))

example :: YieldT Int String String IO String
example = do
  liftIO $ putStrLn "Starting coroutine"
  x <- yield "Send me an Int!"
  liftIO $ putStrLn $ "Got first input: " ++ show x
  y <- yield "Send another Int!"
  liftIO $ putStrLn $ "Got second input: " ++ show y
  return ("Result is: " ++ show (x + y))

runCoroutine :: (Read i, Show o) => YieldT i o r IO r -> IO r
runCoroutine prog = go =<< runYieldT prog
  where
    go (Result r) = return r
    go (Susp o k) = do
      putStrLn $ "Yielded: " ++ show o
      putStrLn "Enter input:"
      i <- read <$> getLine
      k i >>= go

main :: IO ()
main = do
  result <- runCoroutine example
  putStrLn $ "Final result: " ++ result

