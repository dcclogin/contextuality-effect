module Experiment where

import Config
import Context2
import RandomUtils
import State.PaperCheating (sys2, run2)


-- pov of 2 reviewers
-- sys1 :: IO Copy
-- sys2 :: IO (Context Copy)
-- run1 :: Copy -> Context Property -> IO (Context Decision)
-- run2 :: Context Copy -> Context Property -> IO (Context Decision)


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      md = Model {
          copiesOf = sys2
        , reviewersOf = Context (r1, r2)
        , runNonlocal = run2
      }
  getAgreement $ executeModel md


-- Main program
main :: IO ()
main = do
  printStats "(Unknown mechanism)" 18000 runTrial