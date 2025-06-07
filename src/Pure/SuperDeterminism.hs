module Pure.SuperDeterminism where

import Config
import Pure.PaperSomething (inspect)

-- intuition: the statistics can be reconstructed with pure functions
-- not on single property, but on inductively defined query

-- It is called <superdeterminism> which: 
---- reject anything rendered on-the-fly, "by need", and
---- advocate everything including the correlation is precomputed "eagerly"

-- It is mathematically prohibited since:
---- the potential # of contexts are significantly larger than the # of properties (power set)

-- Consider rendering an object in a video game
--- it is insane to compute the rendered 2D image for <every possible camera angle/pov>


-- Source gives the same paper to both reviewers
source :: IO (Paper, Paper)
source = do paper <- randomPaper; return (paper, paper)


supersys1 :: Paper -> Property -> Decision
supersys1 = inspect


inspect2 :: (Paper, Paper) -> (Property, Property) -> IO (Decision, Decision)
inspect2 (paper1, paper2) (p1, p2) = case (inspect paper1 p1, inspect paper2 p2) of
  (dd1, dd2) | p1 == p2 -> return (dd1, dd2)
  (dd1, dd2) | dd1 == dd2 -> do
    ddn <- randomDecision
    return (dd1, ddn) -- (ddn, dd2)
  (dd1, dd2) -> return (dd1, dd2)


runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  (paper1, paper2) <- source
  (d1, d2) <- inspect2 (paper1, paper2) (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)

main :: IO ()
main = do
  printStats "(Pure.SuperDeterminism)" 10000 runTrial