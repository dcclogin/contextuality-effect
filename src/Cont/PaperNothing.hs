module Cont.PaperNothing where

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
source :: IO (Paper, Paper)
source = do paper <- randomPaper; return (paper, paper)

-- blueprint of all papers rendered on-the-fly
thePaper :: Paper
thePaper = Paper Nothing Nothing Nothing


-- [TODO]: instead of wrapping a layer of IO on top of M,
-- define a monad transformer to combine it with IO effects.s
type M = Iterator Paper Paper
-- type M = Iterator (Paper, Property) (Paper, Property)
-- type M = Iterator (Property, Decision) (Property, Decision)


-- render a paper with an ad hoc decision for just one property
renderPaper :: Property -> IO Paper
renderPaper prop = do
  d <- randomDecision
  let paper = case prop of
		Margins   -> thePaper { margins = Just d }
		FontSize  -> thePaper { fontSize = Just d }
		NumPages  -> thePaper { numPages = Just d }
  return paper


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


-- check if any other properties have made a specific decision 
crecallDecision :: Paper -> Property -> (Maybe Decision -> Bool) -> Bool
crecallDecision paper Margins pred =
  pred (fontSize paper) || pred (numPages paper)
crecallDecision paper FontSize pred =
  pred (margins paper) || pred (numPages paper)
crecallDecision paper NumPages pred =
  pred (margins paper) || pred (fontSize paper)


-- the protocol
compromise :: Property -> Paper -> Paper -> Paper -> Decision
compromise prop m1 m2 y = 
  let d1 = getDecision m1 prop
      d2 = getDecision m2 prop
      dy = getDecision y prop in
  case (d1, dy, d2) of
    (Just dd1, Just ddy, _) -> ddy
    (Just dd1, Nothing, Just dd2) | crecallDecision y prop (== d1) -> dd2
    (Just dd1, Nothing, _) -> dd1
    _ -> error "internal bug."   


{--
sys :: Property -> M Decision
sys prop = runYield $ do
  mine <- renderPaper prop
  yours <- yield mine
  return $ compromise ...
--}

sys :: Property -> IO (M Decision)
sys prop = do
  mine1 <- renderPaper prop -- primary rendering
  mine2 <- renderPaper prop -- secondary rendering
  return $ runYield $ do
    -- receive yours from, and send mine1 in the <channel>
    yours <- yield mine1
    return $ compromise prop mine1 mine2 yours


-- TODO: bipartite system

-- inspect2 doesn't even need a input paper
inspect2 :: (Property, Property) -> IO (Decision, Decision)
inspect2 (prop1, prop2) = do
  Susp paper1 k1 <- sys prop1
  Susp paper2 k2 <- sys prop2
  let Result dd1 = k1 paper2
      Result dd2 = k2 paper1 in
    return (dd1, extractDecision paper2)
        -- (extractDecision paper1, dd2)
  where
    extractDecision :: Paper -> Decision
    extractDecision paper = case (margins paper, fontSize paper, numPages paper) of
      (Just dd, _, _) -> dd
      (_, Just dd, _) -> dd
      (_, _, Just dd) -> dd
      _              -> error "internal bug."


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

  putStrLn $ "PaperReview (Continuation, Nothing): Ran " ++ show n ++ " trials.\n"
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

  


