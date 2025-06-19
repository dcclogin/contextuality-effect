module Context2 where

import Control.Applicative
import Data.Foldable
import Data.Traversable

-- homogeneous 2-tuple

newtype Context a
  = Context {unctx :: ((a, a))}
    deriving (Eq, Ord, Bounded, Show, Read)

ctx x0 x1 = Context (x0, x1)
{-# INLINE ctx #-}

instance Functor Context where
  fmap f (Context (x1, x2)) = Context (f x1, f x2)
  {-# INLINABLE fmap #-}

instance Applicative Context where
  pure x = Context (x, x)
  Context (f1, f2) <*> Context (x1, x2) = Context (f1 x1, f2 x2)
  x <* _ = x
  _ *> y = y

instance Monad Context where
  return = pure
  Context (x1, x2) >>= f = 
    Context (case f x1 of Context (y, _) -> y, case f x2 of Context (_, y) -> y)

instance Foldable Context where
  foldr f z (Context (x1, x2)) = f x1 (f x2 z)
  foldl f z (Context (x1, x2)) = f (f z x1) x2

instance Traversable Context where
  traverse f (Context (x1, x2)) = fmap ctx (f x1) <*> (f x2)

