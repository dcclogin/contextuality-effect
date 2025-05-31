module State.Appearance where

import System.Random
import Control.Monad (replicateM)
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Text.Printf


-- just a metaphysical adaption, not a generic version

data Property = P1 | P2 | P3 
    deriving (Eq, Ord, Show, Enum, Bounded)
data Outcome = Yes | No           
    deriving (Eq, Ord, Show)


-- classical object has intrinsic properties
data Object = Object { po1, po2, po3 :: Outcome } deriving (Eq, Show)
-- thing-in-itself can own undefined properties
data Thing = Thing { pt1, pt2, pt3 :: Maybe Outcome } deriving (Eq, Show)


randomOutcome :: IO Outcome
randomOutcome = do b <- randomIO; return $ if b then Yes else No

randomObject :: IO Object
randomObject = Object <$> randomOutcome <*> randomOutcome <*> randomOutcome

-- generate random Thing-in-itself without undefined properties
randomThing :: IO Thing
randomThing = Thing <$> (Just <$> randomOutcome)
                    <*> (Just <$> randomOutcome)
                    <*> (Just <$> randomOutcome)

randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)

sourceObject :: IO (Object, Object)
sourceObject = do obj <- randomObject; return (obj, obj)

sourceThing :: IO (Thing, Thing)
sourceThing = do thing <- randomThing; return (thing, thing)

-- Object as classical hidden variable
-- sys = observe obj :: Property -> Outcome <sys interpreting obs>
observe :: Object -> Property -> Outcome
observe obj p = case p of
  P1 -> po1 obj
  P2 -> po2 obj
  P3 -> po3 obj

-- obs = observable P1 :: Object -> Outcome <obs interpreting sys>
observable :: Property -> Object -> Outcome
observable = flip observe

-- Thing-in-itself as nonlocal hidden variable
type M = State Thing
-- Appearance (for-itself/for-us)
type Appearance = Property -> M Outcome



get1, get2, get3 :: M (Maybe Outcome)
get1 = do thing <- get; return $ pt1 thing
get2 = do thing <- get; return $ pt2 thing
get3 = do thing <- get; return $ pt3 thing

put1, put2, put3 :: (Maybe Outcome) -> M ()
put1 x = do thing <- get; put $ thing { pt1 = x }
put2 x = do thing <- get; put $ thing { pt2 = x }
put3 x = do thing <- get; put $ thing { pt3 = x }

forget1, forget2, forget3 :: M ()
forget1 = put1 Nothing
forget2 = put2 Nothing
forget3 = put3 Nothing

forgetAll :: M ()
forgetAll = do forget1; forget2; forget3

-- conditional forget
cforget1, cforget2, cforget3 :: (Maybe Outcome -> Bool) -> M ()
cforget1 p = do x <- get1; if p x then forget1 else return ()
cforget2 p = do x <- get2; if p x then forget2 else return ()
cforget3 p = do x <- get3; if p x then forget3 else return ()

-- effectful get
getf1, getf2, getf3 :: M (Maybe Outcome)
getf1 = do 
    x <- get1 
    cforget2 (== x)
    cforget3 (== x)
    return x
getf2 = do
    x <- get2 
    cforget1 (== x)
    cforget3 (== x)
    return x
getf3 = do
    x <- get3 
    cforget1 (== x)
    cforget2 (== x)
    return x


-- [TODO] forgetting model applies here