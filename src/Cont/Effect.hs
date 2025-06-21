module Cont.Effect where

import Control.Monad.Cont

data Iterator i o r =
    Result r
  | Susp o (i -> Iterator i o r)

newtype Yield i o r a = Yield { unY :: Cont (Iterator i o r) a }

instance Functor (Yield i o r) where
  fmap f (Yield m) = Yield (fmap f m)

instance Applicative (Yield i o r) where
  pure a = Yield (pure a)
  (Yield mf) <*> (Yield ma) = Yield (mf <*> ma)

instance Monad (Yield i o r) where
  (Yield m) >>= k = Yield (m >>= \a -> unY (k a))

instance MonadCont (Yield i o r) where
  callCC c = Yield (callCC (\k -> unY (c (\a -> Yield (k a)))))

runYield :: Yield i o r r -> Iterator i o r
runYield (Yield m) = runCont m Result

-- yield suspends the current computation, yielding an output of type o,
-- and resumed with an input of type i.
yield :: o -> Yield i o r i
yield o = callCC (\k -> Yield (cont (\_ -> Susp o (\i -> runYield (k i)))))
