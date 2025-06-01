module State.Paper where

import System.Random
import Control.Monad (replicateM)
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Text.Printf

-- The three formatting properties
data Property = Margins | FontSize | NumPages
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Each property passes or fails a rule
data Decision = Fail | Pass
  deriving (Eq, Ord, Show)

-- Quantum paper
data Paper = Paper { margins   :: Maybe Double  -- in inches
                   , fontSize  :: Maybe Double  -- in points
                   , numPages  :: Maybe Int     -- page count
                   } deriving (Eq, Show)

type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)


randomMargin :: IO Double
randomMargin = randomRIO (0.5, 1.5)

randomFontSize :: IO Double
randomFontSize = randomRIO (10.0, 14.0)

randomPageCount :: IO Int
randomPageCount = randomRIO (10, 30)

randomPaper :: IO Paper
randomPaper = Paper <$> (Just <$> randomMargin) 
                    <*> (Just <$> randomFontSize) 
                    <*> (Just <$> randomPageCount)

-- Randomly choose a formatting property
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)

-- Source gives the same paper to both reviewers
source :: IO (Paper, Paper)
source = do paper <- randomPaper; return (paper, paper)


-- nonlocal hidden variable as state monad
type M = State Paper


getMargin :: M (Maybe Double)
getMargin = do paper <- get; return (margins paper)

getFontSize :: M (Maybe Double)
getFontSize = do paper <- get; return (fontSize paper)

getNumPages :: M (Maybe Int)
getNumPages = do paper <- get; return (numPages paper)


putMargin :: Maybe Double -> M ()
putMargin m = do paper <- get; put $ paper { margins = m }

putFontSize :: Maybe Double -> M ()
putFontSize fs = do paper <- get; put $ paper { fontSize = fs }

putNumPages :: Maybe Int -> M ()
putNumPages np = do paper <- get; put $ paper { numPages = np }


forgetMargin, forgetFontSize, forgetNumPages :: M ()
forgetMargin = putMargin Nothing
forgetFontSize = putFontSize Nothing
forgetNumPages = putNumPages Nothing


cforgetMargin :: (Maybe Double -> Bool) -> M ()
cforgetMargin p = do m <- getMargin; if p m then forgetMargin else return ()

cforgetFontSize :: (Maybe Double -> Bool) -> M ()
cforgetFontSize p = do fs <- getFontSize; if p fs then forgetFontSize else return ()

cforgetNumPages :: (Maybe Int -> Bool) -> M ()
cforgetNumPages p = do np <- getNumPages; if p np then forgetNumPages else return ()


-- criteria for Pass/Fail decisions
-- impossible for <Nothing> to appear to the reviewers
passMargin :: Double -> Decision
passMargin m = if abs (m - 1.0) < 0.25 then Pass else Fail

passFontSize :: Double -> Decision
passFontSize fs = if abs (fs - 12.0) < 1.0 then Pass else Fail

passNumPages :: Int -> Decision
passNumPages np = if np < 20 then Pass else Fail

-- [TODO]: come up with a natural way to express forgetting model


-- [TODO]: fill in the main logic for quantum system <appearance>
sys :: Property -> M Decision
sys prop = return Fail

inspect1 :: Paper -> Property -> Decision
inspect1 paper prop = evalState (sys prop) paper

inspect2 :: Paper -> (Property, Property) -> (Decision, Decision)
inspect2 paper (prop1, prop2) =
    let m = do d1 <- sys prop1; d2 <- sys prop2; return (d1, d2) in
        evalState m paper