module Experiment where

import Config
import Context2
import State.PaperOthing (sys, run2)

-- it is the pov of reviewers

{--
runNonlocal :: (Monad m)
            => Context (Property -> m Decision)
            -> Context Property
            -> m (Context Decision)
runNonlocal cs ps = sequence $ cs <*> ps


runContextual :: (Monad m) 
            => (Property -> m Decision) 
            -> Context Property 
            -> m (Context Decision)
runContextual = traverse
--}


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      md = Model {
          copiesOf = return $ Context (sys, sys)
        , reviewersOf = Context (r1, r2)
        , runNonlocal = run2
      }
  getAgreement' $ execNL md


-- Main program
main :: IO ()
main = do
  printStats "(Unknown mechanism)" 20000 runTrial