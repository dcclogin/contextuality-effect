{-# LANGUAGE 
    TypeSynonymInstances
  , FlexibleInstances
  , MultiParamTypeClasses
  , InstanceSigs 
#-}
module Identity.PaperSomething (
  bipartite, runfSeq, runfPar, runContextA, runContextP, label
) where

import Config
import Context2
import Contextuality
import RandomUtils (randomPaperC)
import Control.Monad.Identity


label :: String
label = "(Identity model -- Intrinsic)"


-- Something is hidden
type HiddenVar = PaperC
type M = Identity


src :: IO HiddenVar
src = randomPaperC


runContextA :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextA cs ps = src >>= \s -> runfSeq s cs ps
runContextP :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextP cs ps = src >>= \s -> runfPar s cs ps


-- paper is equivalent to a classical hidden variable
-- Copy is dependent on Paper solely
cp :: PaperC -> Copy M
cp paper = \prop -> case prop of
  Margins  -> return (marginsC paper)
  FontSize -> return (fontSizeC paper)
  NumPages -> return (numPagesC paper)


bipartite :: IO (Context (Copy M))
bipartite = do paper <- src; return $ Context (cp paper, cp paper)
