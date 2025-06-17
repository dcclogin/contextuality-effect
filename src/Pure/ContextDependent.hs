module Pure.ContextDependent where

import Config

-- alternative model of superdeterminism
type HiddenVar = Paper


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


makeCopy :: IO Paper -> IO (Copy, Copy)
makeCopy sc = do paper <- sc; return (cp paper, cp paper)


-- Inspection
inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = return (copy1 prop1, copy2 prop2)


runTrial :: IO ReviewerAgreement
runTrial = do
  prop1 <- randomProperty
  prop2 <- randomProperty
  let r1 = Reviewer (return prop1)
      r2 = Reviewer (return prop2)
      sc = randomPaperCtx (prop1, prop2)
      tr = Trial {
          source = sc
        , copies = makeCopy sc
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr


main :: IO ()
main = do
  printStats "(Pure, Superdeterminism II)" 10000 runTrial


-- paper source depends on choices of properties (context)
-- [TODO] choice of properties depend on paper source 