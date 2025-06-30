module QuantumStates where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM2)
import System.Random (randomRIO)


data Property = X | Y | Z
  deriving (Eq, Show)

data Decision = Zero | One
  deriving (Eq, Show)


flipDecision :: Decision -> Decision
flipDecision Zero = One
flipDecision One  = Zero


---------------------------------------

{----

A quantum state is not stored as a vector but is defined by its behavior under measurement.
So, the Hadamard gate acts on a Qubit m by transforming how it responds to a measurement.

To allow empty question
type Qubit m = Maybe Property -> m (Maybe Decision)

qubitZ0 :: Monad m => Qubit m
qubitZ0 Z = return Zero                       -- definite outcome
qubitZ0 X = uniformDecision                   -- totally uncertain
qubitZ0 Y = uniformDecision                   -- totally uncertain

----}

type Qubit m = Property -> m Decision
type QGate m = Qubit m -> Qubit m

applyGate :: QGate m -> Qubit m -> Qubit m
applyGate = ($)


identity :: QGate m
identity = id

hadamard :: QGate m
hadamard qubit prop = qubit (hPerm prop)
  where
    hPerm Z = X
    hPerm X = Z
    hPerm Y = Y


cnot :: Monad m => Context (Qubit m) -> Context (Qubit m)


class (Applicative f, Monad m) => QuantumSystem f m where
  system   :: f (Qubit m)

