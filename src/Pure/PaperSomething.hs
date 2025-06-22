module Pure.PaperSomething (sys2, run2, label) where

import Config
import Context2
import RandomUtils (randomPaper)
import Control.Monad.Trans.Identity


label :: String
label = "(Identity model -- Intrinsic)"


-- Something is hidden
-- type alias
type HiddenVar = Paper
type M = IdentityT IO

-- a Copy is what a Paper appears/discloses its interface to reviewers
-- a.k.a. what is "observable" of a Paper
-- intuition: an object's identity is determined fully by a collection of predicates
-- related: Leibniz's Law => Observational Equivalence
type Copy = Property -> M Decision


src :: IO HiddenVar
src = randomPaper


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


sys2 :: IO (Context Copy)
sys2 = makeCopy src


-- hiding HiddenVar and export
run1 :: Copy -> Context Property -> IO (Context Decision)
run1 c ps = runIdentityT $ traverse c ps


-- hiding HiddenVar and export
run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = runIdentityT $ sequence $ cs <*> ps
