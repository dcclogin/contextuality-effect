module M where

import Control.Monad.Cont

------------------------------------------------------------------
-- continuation monad (Cont r)

-- runCont :: Cont r a -> (a -> r) -> r
-- cont :: ((a -> r) -> r) -> Cont r a

-- callCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a

ex1 = runCont m id
  where m = do v <- callCC (\k -> do c <- k 3; return (1 + 2 + c + 4))
               return (v+10)

-- 13

-- Example 2

f :: Int -> Bool
f n = g (n+1)

g :: Int -> Bool
g n = h n

h :: Int -> Bool
h n = even n

prog :: Int -> String
prog n = if f n then "True" else "False"

-- now give h the "return address"

fK :: Int -> (Bool -> Cont r b) -> Cont r b
fK n k = gK (n+1) k

gK :: Int -> (Bool -> Cont r b) -> Cont r b
gK n k = hK n k

hK :: Int -> (Bool -> Cont r b) -> Cont r b
hK n k = k (even n)

progK :: Int -> Cont r String
progK n = do x <- callCC (\k -> fK n k)
             if x then return "True" else return "False"

runP :: Int -> String
runP n = runCont (progK n) id

------------------------------------------------------------------
-- Yield
-- Wrapper over continuation monad with special iterator return type

data Iterator i o r =
    Result r
  | Susp o (i -> Iterator i o r)

newtype Yield i o r a = Yield { unY :: Cont (Iterator i o r) a }

{--

fmap :: (a -> b) -> m a -> m b

fmap :: (a -> b) -> Yield i o r a -> Yield i o r b

code below

f :: a -> b

m :: Cont (Iterator i o r) a

want :: Cont (Iterator i o r) b

use fmap from Cont library

--}


instance Functor (Yield i o r) where
  fmap f (Yield m) = Yield (fmap f m)

instance Applicative (Yield i o r) where
  pure a = Yield (pure a)
  (Yield mf) <*> (Yield ma) = Yield (mf <*> ma)

instance Monad (Yield i o r) where
  return a = Yield (return a)
  (Yield m) >>= k = Yield (m >>= \a -> unY (k a))

instance MonadCont (Yield i o r) where
  callCC c = Yield (callCC (\k -> unY (c (\a -> Yield (k a)))))

runYield :: Yield i o r r -> Iterator i o r
runYield (Yield m) = runCont m Result

yield :: o -> Yield i o r i
yield o = callCC (\k -> Yield (cont (\_ -> Susp o (\i -> runYield (k i)))))

--

testY1 :: Iterator Bool Int ()
testY1 = runYield $ do
           _ <- yield 1
           _ <- yield 2
           b <- yield 3
           if b then do yield 4; yield 5; yield 6; return ()
                else return ()
   
{--

M> let Susp v1 k1 = testY1

M> v1
1

M> let Susp v2 k2 = k1 True

M> v2
2

M> let Susp v3 k3 = k2 True

M> v3
3

M> let Result () = k3 False

--}

-- returns successive Fibonnaci numbers

testY2 :: Iterator Bool Int Int 
testY2 = runYield (loop (0,1))
  where loop (a,b) = do c <- yield a
                        if c then loop (b, a+b) else return a

{--

M> let Susp v1 k1 = fibIter

M> v1
0

M> let Susp v2 k2 = k1 True

M> v2
1

M> let Susp v3 k3 = k2 True

M> v3
1

M> let Susp v4 k4 = k3 True

M> v4
2

M> let Susp v5 k5 = k4 True

M> v5
3

--}

-- useful combinators for interacting with iterators

toList :: Iterator Bool o Int -> [o]
toList (Result _) = []
toList (Susp o k) = o : toList (k True)

{--

M> toList testY1
[1,2,3,4,5,6]

--}

while :: (o -> Bool) -> Iterator Bool o r -> r
while f (Result r) = r
while f (Susp o k) = while f (k (f o))

{--

M> while (<= 100) testY2
144

--}

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving Show

--

depthWalk :: Tree o -> Yield a o r (Tree a)
depthWalk (Leaf a) = do b <- yield a
                        return (Leaf b)
depthWalk (Node t1 t2) = do t1' <- depthWalk t1
                            t2' <- depthWalk t2
                            return (Node t1' t2')


foreach :: (o -> i) -> Iterator i o r -> r
foreach f (Result r) = r
foreach f (Susp o k) = foreach f (k (f o))

renumber :: Tree Int
renumber = foreach (+1) (runYield (depthWalk tr))
  where tr = Node (Node (Leaf 10) (Leaf 20)) (Leaf 30)

--

leaves :: Tree o -> Yield () o () ()
leaves (Leaf a) = do yield a; return ()
leaves (Node t1 t2) = do leaves t1; leaves t2

sameFringe :: Tree Int -> Tree Int -> Bool
sameFringe t1 t2 = loop (runYield (leaves t1)) (runYield (leaves t2))
  where loop (Result ()) (Result ()) = True
        loop (Susp v1 k1) (Susp v2 k2) | v1 /= v2 = False
                                       | otherwise = loop (k1 ()) (k2 ())
          
tr1, tr2, tr3 :: Tree Int
tr1 = Node (Node (Leaf 1) (Leaf 2)) (Leaf 3)
tr2 = Node (Leaf 1) (Node (Leaf 2) (Leaf 3))
tr3 = Node (Leaf 4) (Node (Leaf undefined) (Leaf undefined))

--------------------------------------------------------------------------------
-- Logic

data Empty = Empty

type Not a = a -> Empty

deMorgan1 :: (Not a, Not b) -> Not (Either a b)
deMorgan1 (na,nb) (Left a) = na a
deMorgan1 (na,nb) (Right b) = nb b

deMorgan2 :: Not (Either a b) -> (Not a, Not b) 
deMorgan2 nab = (\a -> nab (Left a), \b -> nab (Right b))

contraPositive :: (a -> b) -> (Not b -> Not a)
contraPositive f nb a = nb (f a)

--

data R a c = R (a -> Yield a (R a c) c c)

{--

(R a c) is a process that expects a value of type a; 
it is an encoding of (Not a) 

keeping in mind that a -> b = Either (Not a) b
encode f :: a -> b as a process that immediately produces 
something simiilar to (Not a); more precisely we produce
a demand for 'a' (by yielding something of type R a c)

when this process is resumed with a value of type 'a', 
it changes its mind and produces a value of type b

--}

strange :: (a -> b) -> Yield a (R a c) c b
strange f = do a <- callCC (\k -> yield (R k)) 
               return (f a)

encodedEven :: Iterator Int (R Int Bool) Bool
encodedEven = runYield (strange even)

testEven :: Int -> Bool
testEven a = 
  case encodedEven of 
    Susp (R k) _ -> case runYield (k a) of Result b -> b

--------------------------------------------------------------------------------
