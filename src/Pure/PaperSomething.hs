module Pure.PaperSomething where

import Config
import Control.Monad.Identity


-- Something is hidden
type HiddenVar = Paper


-- a Copy is what a Paper appears/discloses its interface to reviewers
-- a.k.a. what is "observable" of a Paper
-- intuition: an object's identity is determined fully by a collection of predicates
-- related: Leibniz's Law => Observational Equivalence
type Copy = Property -> Decision


-- paper is equivalent to a classical hidden variable
cp :: Paper -> Copy
cp paper = \prop -> case getDecision paper prop of
  Just dec -> dec
  Nothing  -> error "internal bug." 
  where
    getDecision :: Paper -> Property -> Maybe Decision
    getDecision paper prop = case prop of
      Margins   -> margins paper
      FontSize  -> fontSize paper
      NumPages  -> numPages paper


obs :: Property -> Paper -> Decision
obs = flip cp


makeCopy :: IO Paper -> IO (Copy, Copy)
makeCopy sc = do paper <- sc; return (cp paper, cp paper)


inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) = return (copy1 prop1, copy2 prop2)


execute :: Trial HiddenVar Copy -> IO (Outcome, Outcome)
execute tr = do
  hvar <- source tr
  (copy1, copy2) <- copies tr
  prop1 <- choice $ fst $ reviewers tr
  prop2 <- choice $ snd $ reviewers tr
  (dec1, dec2) <- inspect hvar (copy1, copy2) (prop1, prop2)
  return (Outcome prop1 dec1, Outcome prop2 dec2)


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      sc = randomPaper
      tr = Trial {
          source = sc
        , copies = makeCopy sc
        , reviewers = (r1, r2)
      } 
  getAgreement $ execute tr


main :: IO ()
main = do
  printStats "(Pure)" 10000 runTrial
