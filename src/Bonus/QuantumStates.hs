module QuantumStates where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (liftM2)
import System.Random (randomRIO)


{----

A quantum state is not stored as a vector but is defined by its behavior under measurement.
So, the Hadamard gate acts on a Qubit m by transforming how it responds to a measurement.

data (Monad m) => Qubit m = Qubit (Maybe Property -> Either (m Decision) (Qubit m))

----}




