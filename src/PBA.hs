module PBA where

import Config


-- Query is a partial Boolean algebra
data Query = FF | TT
  | Expected Outcome
  | Not Query
  | And Query Query
  | Or Query Query
  deriving (Eq, Show)


-- [TODO]: implement the binary relation following the paper <the logic of contextuality>
isCommeasurable :: Query -> Query -> Bool
isCommeasurable q1 q2 = False