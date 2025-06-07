module PBA where

import Config

-- Query is a partial Boolean algebra
data Query = P Property
  | Not Query
  | And Query Query
  | Or Query Query
  deriving (Eq, Show)


-- [TODO]: implement the binary relation following the paper <the logic of contextuality>
isCommesurable :: Query -> Query -> Bool
isCommesurable q1 q2 = False