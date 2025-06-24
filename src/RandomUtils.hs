module RandomUtils (
  randomDecision
, randomPaper
, randomPaper0
, randomPaperCtx
, randomProperty
, randomDecisionSame
, randomDecisionDiff) where

import Config
import Context2
import System.Random

-- mediator between reviewer and paper
-- plays significant role in superdeterminism models


-- generate a random decision
randomDecision :: IO Decision
randomDecision = do b <- randomIO; return $ notD b


-- generate a random paper with random decisions for each property
randomPaper :: IO Paper
randomPaper = Paper <$> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision) 
                    <*> (Just <$> randomDecision)


-- generate a random paper (with 0 probability for PPP and FFF)
randomPaper0 :: IO Paper
randomPaper0 = do
  (dec1, dec2, dec3) <- randomDecision3
  return $ Paper (Just dec1) (Just dec2) (Just dec3)
  where
    randomDecision3 :: IO (Decision, Decision, Decision)
    randomDecision3 = do
      b1 <- randomIO
      b2 <- randomIO
      b3 <- randomIO
      if (b1 == b2) && (b2 == b3)
        then randomDecision3
        else return (reg b1, reg b2, reg b3)


-- randomly choose a formatting property
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)


randomDecisionSame, randomDecisionDiff :: IO (Decision, Decision)
randomDecisionSame = do dec <- randomDecision; return (dec, dec)
randomDecisionDiff = do 
  dec1 <- randomDecision
  dec2 <- randomDecision
  if dec1 /= dec2
    then return (dec1, dec2)
    else do dec <- randomDecision; return (dec, dec2) -- return (dec1, dec)


-- random paper yet depending on choices of properties
randomPaperCtx :: Context Property -> IO Paper
randomPaperCtx (Context (prop1, prop2)) = do
  if prop1 == prop2
    then do
      (dec1, dec2) <- randomDecisionSame
      return $ genPaper1 thePaper prop1 dec1
    else do
      (dec1, dec2) <- randomDecisionDiff
      return $ genPaper2 thePaper (prop1, prop2) (dec1, dec2)
  where
    genPaper1 :: Paper -> Property -> Decision -> Paper
    genPaper1 blueprint prop dec = case prop of
      Margins  -> blueprint { margins = Just dec }
      FontSize -> blueprint { fontSize = Just dec }
      NumPages -> blueprint { numPages = Just dec }

    genPaper2 :: Paper -> (Property, Property) -> (Decision, Decision) -> Paper
    genPaper2 blueprint (prop1, prop2) (dec1, dec2) = 
      genPaper1 (genPaper1 blueprint prop1 dec1) prop2 dec2