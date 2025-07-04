module Identity.ContextDependent (
  makeBipartite, dependentSrc, runfSeq, runfPar, label
) where

import Config
import Context2
import Contextuality
import RandomUtils (randomPaperCtx)
import Control.Monad.Identity


-- alternative model of superdeterminism
label :: String
label = "(Superdeterminism -- context dependent source)"


type HiddenVar = PaperC
type M = Identity


-- must be interdependent under the same IO environment
dependentSrc :: Context Property -> IO HiddenVar
dependentSrc = randomPaperCtx


cp :: PaperC -> Copy M
cp paper = \prop -> case prop of
  Margins  -> return (marginsC paper)
  FontSize -> return (fontSizeC paper)
  NumPages -> return (numPagesC paper)


-- obs :: Property -> Paper -> M Decision
-- obs = flip cp


makeBipartite :: IO PaperC -> IO (Context (Copy M))
makeBipartite src = do paper <- src; return $ Context (cp paper, cp paper)


-- paper source depends on choices of properties
-- choice of properties depend on paper source 