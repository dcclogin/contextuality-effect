module MerminCubes where

import System.Random
import System.Environment (getArgs)
import Control.Monad (replicateM)
import qualified Data.Map.Strict as Map
import Text.Printf

data Dimension = Width | Length | Height deriving (Eq, Ord, Show, Enum, Bounded)
data Color     = Red | Green           deriving (Eq, Ord, Show)

data Cube = Cube { widthC, lengthC, heightC :: Color } deriving (Eq, Show)

type Classified = (Bool, Bool)  -- (sameSetting, sameColor)

-- Generate a random cube
randomCube :: IO Cube
randomCube = Cube <$> randomColor <*> randomColor <*> randomColor

randomColor :: IO Color
randomColor = do b <- randomIO; return $ if b then Red else Green

randomDimension :: IO Dimension
randomDimension = toEnum <$> randomRIO (0, 2)

-- Same cube to both detectors (classical hidden variable)
source :: IO (Cube, Cube)
source = do cube <- randomCube; return (cube, cube)

measure :: Cube -> Dimension -> Color
measure cube dim = case dim of
  Width  -> widthC cube
  Length -> lengthC cube
  Height -> heightC cube

-- Run one trial and classify it
runTrial :: IO Classified
runTrial = do
  d1 <- randomDimension
  d2 <- randomDimension
  (c1, c2) <- source
  let sameSetting = d1 == d2
      sameColor   = measure c1 d1 == measure c2 d2
  return (sameSetting, sameColor)

-- Run trials and count (sameSetting, sameColor) outcomes
runClassified :: Int -> IO (Map.Map Classified Int)
runClassified n = do
  results <- replicateM n runTrial
  return $ Map.fromListWith (+) [ (r, 1) | r <- results ]

-- Display results with conditional percentages
main :: IO ()
main = do
  args <- getArgs
  let n = maybe 10000 read (listToMaybe args)

  counts <- runClassified n

  let total same = sum [ c | ((s, _), c) <- Map.toList counts, s == same ]
      getPct same color = 
        let count = Map.findWithDefault 0 (same, color) counts
        in if total same == 0 then 0 else fromIntegral count * 100 / fromIntegral (total same) :: Double

  putStrLn $ "Ran " ++ show n ++ " trials.\n"
  putStrLn "Category                Percent"
  printEntry "SameSetting & SameColor" (getPct True  True)
  printEntry "SameSetting & DiffColor" (getPct True  False)
  printEntry "DiffSetting & SameColor" (getPct False True)
  printEntry "DiffSetting & DiffColor" (getPct False False)

  where
    printEntry label pct = putStrLn $ padRight 26 label ++ showFF pct ++ " %"
    padRight n s = s ++ replicate (n - length s) ' '
    showFF = printf "%.2f"
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

