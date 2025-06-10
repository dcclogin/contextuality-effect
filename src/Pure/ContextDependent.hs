module Pure.ContextDependent where

import Config

-- alternative model of superdeterminism


-- blueprint for all papers
thePaper :: Paper
thePaper = Paper Nothing Nothing Nothing


randomDecisionSame, randomDecisionDiff :: IO (Decision, Decision)
randomDecisionSame = do dec <- randomDecision; return (dec, dec)
randomDecisionDiff = do 
  dec1 <- randomDecision
  dec2 <- randomDecision
  if dec1 /= dec2
    then return (dec1, dec2)
    else do dec <- randomDecision; return (dec, dec2) -- return (dec1, dec)


genPaper1 :: Paper -> Property -> Decision -> Paper
genPaper1 blueprint prop dec = case prop of
  Margins  -> blueprint { margins = Just dec }
  FontSize -> blueprint { fontSize = Just dec }
  NumPages -> blueprint { numPages = Just dec }


genPaper2 :: Paper -> (Property, Property) -> (Decision, Decision) -> Paper
genPaper2 blueprint (prop1, prop2) (dec1, dec2) = 
  genPaper1 (genPaper1 blueprint prop1 dec1) prop2 dec2


type Context = (Property, Property)


-- context dependent state|paper
randomPaperCtx :: Context -> IO Paper
randomPaperCtx (prop1, prop2) = do
  if prop1 == prop2
    then do
      (dec1, dec2) <- randomDecisionSame
      return $ genPaper1 thePaper prop1 dec1
    else do
      (dec1, dec2) <- randomDecisionDiff
      return $ genPaper2 thePaper (prop1, prop2) (dec1, dec2)


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


type Copy = Property -> Decision


-- paper is equivalent to a classical hidden variable
cp :: Paper -> Copy
cp paper = \prop -> case getDecision paper prop of
  Just dec -> dec
  Nothing  -> error "internal bug." 


makeCopy :: Paper -> IO (Copy, Copy)
makeCopy paper = return (cp paper, cp paper)


-- Inspection
inspect :: Copy -> Property -> IO Decision
inspect copy prop = return $ copy prop


inspect2 :: (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect2 (copy1, copy2) (prop1, prop2) = do
  dec1 <- inspect copy1 prop1
  dec2 <- inspect copy2 prop2
  return (dec1, dec2)


-- Run a single trial
runTrial :: IO ReviewerAgreement
runTrial = do
  p1 <- randomProperty
  p2 <- randomProperty
  paper <- randomPaperCtx (p1, p2)
  (copy1, copy2) <- makeCopy paper
  (d1, d2) <- inspect2 (copy1, copy2) (p1, p2)
  let sameProperty = p1 == p2
      sameDecision = d1 == d2
  return (sameProperty, sameDecision)


main :: IO ()
main = do
  printStats "(Pure, Superdeterminism II)" 10000 runTrial


-- paper source depends on choices of properties (context)
-- [TODO] choice of properties depend on paper source 