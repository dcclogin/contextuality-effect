module Pure.ContextDependent where

import Config
import Context2
import RandomUtils (randomPaperCtx)
import Control.Monad.Trans.Identity

-- alternative model of superdeterminism

type HiddenVar = Paper
type M = IdentityT IO
type Copy = Property -> M Decision


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


-- hiding HiddenVar and export
run1 :: Copy -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- dependentSrc ps
  runIdentityT (traverse c ps)


-- hiding HiddenVar and export
run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- dependentSrc ps
  runIdentityT (sequence $ cs <*> ps)


{--
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
--}

-- paper source depends on choices of properties (context)
-- [TODO] choice of properties depend on paper source 