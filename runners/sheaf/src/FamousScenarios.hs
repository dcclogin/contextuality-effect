module FamousScenarios where

import MeasurementScenario

--------------------------------------------------------------------------------
-- GHZ (3-qubit) Strong Contextuality
--------------------------------------------------------------------------------

ghz :: ExperimentDef Observable Outcome
ghz = experiment
  [ context ["Ax", "By", "Cy"]  (parityConstraint 1)
  , context ["Ay", "Bx", "Cy"]  (parityConstraint 1)
  , context ["Ay", "By", "Cx"]  (parityConstraint 1)
  , context ["Ax", "Bx", "Cx"]  (parityConstraint (-1))
  ]

--------------------------------------------------------------------------------
-- Mermin Square (Peres–Mermin Magic Square)
-- 3x3 grid with row/column constraints
--------------------------------------------------------------------------------

merminSquare :: ExperimentDef Observable Outcome
merminSquare = experiment
  [ context ["X1", "X2", "X3"]     (parityConstraint 1)
  , context ["Y1", "Y2", "Y3"]     (parityConstraint 1)
  , context ["Z1", "Z2", "Z3"]     (parityConstraint 1)
  , context ["X1", "Y1", "Z1"]     (parityConstraint 1)
  , context ["X2", "Y2", "Z2"]     (parityConstraint 1)
  , context ["X3", "Y3", "Z3"]     (parityConstraint (-1))
  ]

--------------------------------------------------------------------------------
-- Hardy's Paradox (non-locality without inequalities)
--------------------------------------------------------------------------------

hardy :: ExperimentDef Observable Outcome
hardy = experiment
  [ context ["A0", "B0"] (sumConstraint 2)  -- A0=1 and B0=1 happens
  , probabilisticContext                    -- A0=1 and B1=1 forbidden
      ["A0", "B1"]
      (\[a, b] -> a == 1 && b == 1)
      (\[a, b] -> not (a == 1 && b == 1))
  , context ["A1", "B0"] (sumConstraint 1)  -- implies A1=1 ⇒ B0=1
  , context ["A1", "B1"] (sumConstraint 0)  -- cannot both be 1
  ]

--------------------------------------------------------------------------------
-- KCBS: Cyclic contextuality (5-cycle)
--------------------------------------------------------------------------------

kcbs :: ExperimentDef Observable Outcome
kcbs = experiment
  [ context ["A1", "A2"] (sumConstraint 1)
  , context ["A2", "A3"] (sumConstraint 1)
  , context ["A3", "A4"] (sumConstraint 1)
  , context ["A4", "A5"] (sumConstraint 1)
  , context ["A5", "A1"] (sumConstraint 1)
  ]

--------------------------------------------------------------------------------
-- Specker’s Triangle: 3 observables, pairwise compatible
--------------------------------------------------------------------------------

speckerTriangle :: ExperimentDef Observable Outcome
speckerTriangle = experiment
  [ context ["A", "B"] (allEqualConstraint)
  , context ["B", "C"] (allEqualConstraint)
  , context ["C", "A"] (allEqualConstraint)
  ]

--------------------------------------------------------------------------------
-- | Mermin 3-qubit device: all contexts have product = -1
-- Appears locally consistent, but globally inconsistent
--------------------------------------------------------------------------------

mermin3Qubit :: ExperimentDef Observable Outcome
mermin3Qubit = experiment
  [ context ["X1", "Y2", "Y3"] (parityConstraint (-1))
  , context ["Y1", "X2", "Y3"] (parityConstraint (-1))
  , context ["Y1", "Y2", "X3"] (parityConstraint (-1))
  , context ["X1", "X2", "X3"] (parityConstraint (-1))
  ]

--------------------------------------------------------------------------------
-- | PR-box logic translated to parity constraints
--------------------------------------------------------------------------------

prBox :: ExperimentDef Observable Outcome
prBox = experiment
  [ context ["A0", "B0"] (parityConstraint 1)   -- x=0, y=0 → same
  , context ["A0", "B1"] (parityConstraint 1)   -- x=0, y=1 → same
  , context ["A1", "B0"] (parityConstraint 1)   -- x=1, y=0 → same
  , context ["A1", "B1"] (parityConstraint (-1))-- x=1, y=1 → opposite
  ]

--------------------------------------------------------------------------------  