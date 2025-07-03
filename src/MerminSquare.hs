module MerminSquare where

-- prototyping Mermin square

type Decision = Bool

-- a row is allowed to have odd number of True
rowProperty :: Decision -> Decision -> Decision -> Bool
rowProperty d1 d2 d3 = if d1 then d2 == d3 else d2 /= d3

-- a column is allowed to have even number of True
colProperty :: Decision -> Decision -> Decision -> Bool
colProperty d1 d2 d3 = if d1 then d2 /= d3 else d2 == d3