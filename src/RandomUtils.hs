module RandomUtils (
  randomDecision
, randomPaper
, randomPaperC
, randomPaper0
, randomPaperCtx
, randomProperty
, randomDecisionSame
, randomDecisionDiff) where

import Config
import Context2
import System.Random
import Control.Concurrent.Async (race)

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

randomPaperC :: IO PaperC
randomPaperC = PaperC <$> randomDecision <*> randomDecision <*> randomDecision


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
        else return (notD b1, notD b2, notD b3)


-- randomly choose a formatting property
randomProperty :: IO Property
randomProperty = toEnum <$> randomRIO (0, 2)


-- different random decision for:
---- when two properties are the same
---- when two properties are different
randomDecisionSame, randomDecisionDiff :: IO (Decision, Decision)
randomDecisionSame = do dec <- randomDecision; return (dec, dec)
randomDecisionDiff = do 
  dec1 <- randomDecision
  dec2 <- randomDecision
  if dec1 /= dec2
    then return (dec1, dec2)
    else do 
      dec <- randomDecision
      winner <- race (return (dec, dec2)) (return (dec1, dec))
      case winner of
        Left res -> return res
        Right res -> return res


-- random paper yet depending on choices of properties
randomPaperCtx :: Context Property -> IO PaperC
randomPaperCtx (Context (prop1, prop2)) = do
  if prop1 == prop2
    then randomPaperC
    else do
      dec <- randomDecision
      (dec1, dec2) <- randomDecisionDiff
      return $ buildPaperC (prop1, prop2) (dec1, dec2) dec
  where
    buildPaperC (p1, p2) (d1, d2) d3 =
      PaperC { marginsC  = pick Margins
             , fontSizeC = pick FontSize
             , numPagesC = pick NumPages }
      where
        pick :: Property -> Decision
        pick prop
          | prop == p1 = d1
          | prop == p2 = d2
          | otherwise  = d3