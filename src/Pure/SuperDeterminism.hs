module Pure.SuperDeterminism where

import Config
-- import Pure.PaperSomething (inspect)

-- intuition: the statistics can be reconstructed with pure functions
-- not on single property, but on inductively defined query

-- It corresponds to <superdeterminism> which: 
---- reject anything rendered on-the-fly, "by need", and
---- advocate everything including the correlation is precomputed "eagerly"

-- It is mathematically prohibited since:
---- the potential # of contexts are significantly larger than the # of properties (power set)

-- Consider rendering an object in a video game:
---- it is insane to precompute the rendered 2D image for <every possible camera angle/pov>


data SuperPaper = SuperPaper {
    marginsANDmargins   :: (Decision, Decision)
  , fontSizeANDfontSize :: (Decision, Decision)
  , numPagesANDnumPages :: (Decision, Decision)
  , marginsANDfontSize  :: (Decision, Decision)
  , marginsANDnumPages  :: (Decision, Decision)
  , fontSizeANDnumPages :: (Decision, Decision)
} deriving (Eq, Show)


{--
data CorrPaper = CorrPaper {
  marginsC :: Decision
  fontSizeC :: Decision
  numPagesC :: Decision
  corrSame :: Double 
  corrDiff :: Double
}
--}


randomDecisionSame, randomDecisionDiff :: IO (Decision, Decision)
randomDecisionSame = do dec <- randomDecision; return (dec, dec)
randomDecisionDiff = do 
  dec1 <- randomDecision
  dec2 <- randomDecision
  if dec1 /= dec2
    then return (dec1, dec2)
    else do dec <- randomDecision; return (dec, dec2) -- return (dec1, dec)


randomSuperPaper :: IO SuperPaper
randomSuperPaper = SuperPaper <$> randomDecisionSame <*> randomDecisionSame <*> randomDecisionSame
                              <*> randomDecisionDiff <*> randomDecisionDiff <*> randomDecisionDiff


{--
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
--}


source :: IO (SuperPaper, SuperPaper)
source = do superpaper <- randomSuperPaper; return (superpaper, superpaper)


inspect :: SuperPaper -> (Property, Property) -> (Decision, Decision)
inspect superpaper (p1, p2) = case (p1, p2) of
  (Margins, Margins) -> marginsANDmargins superpaper
  (FontSize, FontSize) -> fontSizeANDfontSize superpaper
  (NumPages, NumPages) -> numPagesANDnumPages superpaper
  (Margins, FontSize) -> marginsANDfontSize superpaper
  (FontSize, Margins) -> marginsANDfontSize superpaper
  (Margins, NumPages) -> marginsANDnumPages superpaper
  (NumPages, Margins) -> marginsANDnumPages superpaper
  (FontSize, NumPages) -> fontSizeANDnumPages superpaper
  (NumPages, FontSize) -> fontSizeANDnumPages superpaper


runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  superpaper <- randomSuperPaper
  let (d1, d2) = inspect superpaper (p1, p2)
      sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)


main :: IO ()
main = do
  printStats "(Pure, Superdeterminism)" 10000 runTrial