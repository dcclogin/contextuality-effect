module Pure.ContextDependent (dependentSrc, makeCopy, run1S, run2S, label) where

import Config
import Context2
import RandomUtils (randomPaperCtx)
import Control.Monad.Trans.Identity

-- alternative model of superdeterminism
label :: String
label = "(Superdeterminism -- context dependent source)"


type HiddenVar = Paper
type M = IdentityT IO
type Copy = Property -> M Decision


-- must be interdependent under the same IO environment
dependentSrc :: Context Property -> IO HiddenVar
dependentSrc = randomPaperCtx


-- paper is equivalent to a classical hidden variable
-- Copy is dependent on Paper solely
cp :: Paper -> Copy
cp paper = \prop -> case getDecision paper prop of
  Just dec -> return dec
  Nothing  -> error "internal bug."
  where
    getDecision :: Paper -> Property -> Maybe Decision
    getDecision paper prop = case prop of
      Margins   -> margins paper
      FontSize  -> fontSize paper
      NumPages  -> numPages paper


obs :: Property -> Paper -> M Decision
obs = flip cp


makeCopy :: IO Paper -> IO (Context Copy)
makeCopy s = do paper <- s; return $ Context (cp paper, cp paper)


run1S :: HiddenVar -> Copy -> Context Property -> IO (Context Decision)
run1S hvar c ps = runIdentityT $ traverse c ps


run2S :: HiddenVar -> Context Copy -> Context Property -> IO (Context Decision)
run2S hvar cs ps = runIdentityT $ sequence $ cs <*> ps


-- paper source depends on choices of properties (context)
-- [TODO] choice of properties depend on paper source 