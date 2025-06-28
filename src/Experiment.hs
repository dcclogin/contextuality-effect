module Experiment where

import Config
import Context2
import RandomUtils
import Cont.PaperOthing


-- pov of 2 reviewers:
-- sys1 :: IO Copy
-- sys2 :: IO (Context Copy)
-- run1 :: Copy -> Context Property -> IO (Context Decision)
-- run2 :: Context Copy -> Context Property -> IO (Context Decision)

-- the reviewers should not know:
-- src and HiddenVar
-- whether two copies are the same or not


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      md = Model {
          copies = sys2
        , reviewers = Context (r1, r2)
        , runContext = run2
      }
  getAgreement $ executeModel md


-- Main program
main :: IO ()
main = do
  printStats label 18367 runTrial