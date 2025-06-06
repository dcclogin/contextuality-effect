module Cont.PaperOthing where

import System.Random
import System.Environment (getArgs)
import Control.Monad (replicateM)
import Cont.Effect
import Control.Monad.Cont
import qualified Data.Map.Strict as Map
import Text.Printf

-- The three formatting properties
data Property = Margins | FontSize | NumPages
  deriving (Eq, Ord, Show, Enum, Bounded)

-- Each property passes or fails a rule
data Decision = Fail | Pass
  deriving (Eq, Ord, Show)

-- Quantum paper
data Paper = Paper { margins   :: Maybe Decision
                   , fontSize  :: Maybe Decision
                   , numPages  :: Maybe Decision
                   } deriving (Eq, Show)

type ReviewerAgreement = (Bool, Bool)  -- (sameProperty, sameDecision)


randomDecision :: IO Decision
randomDecision = do
  b <- randomIO
  return $ if b then Pass else Fail

randomPaper :: IO Paper
randomPaper = Paper <$> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision)

-- Randomly choose a formatting property
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)

-- Source gives the same paper to both reviewers
-- source :: IO (Paper, Paper)
-- source = do paper <- randomPaper; return (paper, paper)

-- blueprint of all papers rendered on-the-fly
-- thePaper :: Paper
-- thePaper = Paper Nothing Nothing Nothing

-- Intuition: if thePaper is nothing, then we don't really need a blueprint.

-- [TODO]: instead of wrapping a layer of IO on top of M,
-- define a monad transformer to combine it with IO effects.s
-- type M = Iterator Paper Paper
-- type M = Iterator (Paper, Property) (Paper, Property)
type Pixel = (Property, Decision)
type M = Iterator Pixel Pixel


-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> IO Pixel
renderPixel prop = do d <- randomDecision; return (prop, d)


-- render a paper with an ad hoc decision for just one property
protocol :: Pixel -> Pixel -> Pixel -> Decision
protocol py p1 (_, dd2) = 
  case (py, p1) of
    ((propy, ddy), (propm, dd1)) | propy == propm -> ddy
    ((propy, ddy), (propm, dd1)) | ddy == dd1 -> dd2
    ((propy, ddy), (propm, dd1)) | ddy /= dd1 -> dd1
    _ -> error "internal bug."


sys :: Property -> IO (M Decision)
sys prop = do
  mine1 <- renderPixel prop -- primary rendering
  mine2 <- renderPixel prop -- secondary rendering
  return $ runYield $ do
    yours <- yield mine1 -- receive yours; send mine1
    return $ protocol yours mine1 mine2


inspect2 :: (Property, Property) -> IO (Decision, Decision)
inspect2 (prop1, prop2) = do
  Susp pixel1 k1 <- sys prop1
  Susp pixel2 k2 <- sys prop2
  let Result dd1 = k1 pixel2
      Result dd2 = k2 pixel1 in
    return (dd1, snd pixel2)
        -- (snd pixel1, dd2)


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  -- paper <- randomPaper
  (d1, d2) <- inspect2 (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)


-- Collect statistics
runReviewerAgreement :: Int -> IO (Map.Map ReviewerAgreement Int)
runReviewerAgreement n = do
  results <- replicateM n runTrial
  return $ Map.fromListWith (+) [ (r, 1) | r <- results ]

-- Main program
main :: IO ()
main = do
  args <- getArgs
  let n = maybe 10000 read (listToMaybe args)

  counts <- runReviewerAgreement n

  let total same = sum [ c | ((s, _), c) <- Map.toList counts, s == same ]
      getPct same agree =
        let count = Map.findWithDefault 0 (same, agree) counts
        in if total same == 0 then 0 else fromIntegral count * 100 / fromIntegral (total same) :: Double

  putStrLn $ "PaperReview (Continuation, Othing): Ran " ++ show n ++ " trials.\n"
  putStrLn "Category                          Percent"
  printEntry "SameProperty & SameDecision" (getPct True  True)
  printEntry "SameProperty & DiffDecision" (getPct True  False)
  printEntry "DiffProperty & SameDecision" (getPct False True)
  printEntry "DiffProperty & DiffDecision" (getPct False False)

  where
    printEntry label pct = putStrLn $ padRight 35 label ++ showFF pct ++ " %"
    padRight n s = s ++ replicate (n - length s) ' '
    showFF = printf "%.2f"
    listToMaybe []    = Nothing
    listToMaybe (x:_) = Just x

  


