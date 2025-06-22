module ExperimentS where

import Config
import Context2
import RandomUtils
import Pure.ContextDependent


-- objective pov


runTrialS :: IO ReviewerAgreement
runTrialS = do
  prop1 <- randomProperty
  prop2 <- randomProperty
  let r1 = Reviewer (return prop1)
      r2 = Reviewer (return prop2)
      sc = dependentSrc $ Context (prop1, prop2)
      md = Model {
          source = sc
        , copies = makeCopy sc
        , reviewers = Context (r1, r2)
        , runContextS = run2S
      }
  getAgreement $ executeModelS md


-- Main program
main :: IO ()
main = do
  printStats label 18367 runTrialS