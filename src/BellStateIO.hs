module BellStateIO where

-- import Context2
import Control.Monad (replicateM)
import System.Random (randomIO)

-- Measurement properties (basis)
data Property = X | Y | Z deriving (Eq, Show)

-- Measurement outcomes
data Decision = Zero | One deriving (Eq, Show)

-- Homogeneous 2-tuple for two-qubit context
newtype Context a = Context (a, a) deriving (Eq, Show)

-- Flip a decision
flipDecision :: Decision -> Decision
flipDecision Zero = One
flipDecision One  = Zero

-- Uniformly random Zero or One
uniformDecision :: IO Decision
uniformDecision = do
  b <- randomIO
  return $ if b then Zero else One

-- Qubit is a function from Property to IO Decision
type Qubit = Property -> IO Decision

definiteIn :: Property -> Decision -> Qubit
definiteIn basis val p
  | p == basis = return val
  | otherwise  = uniformDecision

qubit0 :: Qubit
qubit0 = definiteIn Z Zero

-- Hadamard gate swaps Z and X, leaves Y
hPerm :: Property -> Property
hPerm Z = X
hPerm X = Z
hPerm Y = Y

hadamard :: Qubit -> Qubit
hadamard qubit prop = qubit (hPerm prop)

-- CNOT logic on classical decisions
cnotDecisions :: Context Decision -> Context Decision
cnotDecisions (Context (ctrl, tgt)) =
  Context (ctrl, if ctrl == One then flipDecision tgt else tgt)

-- Apply CNOT to two qubits via measurement
type QubitPair = Context Qubit

type Measurement = Context Property -> IO (Context Decision)

cnotQubits :: QubitPair -> Measurement
cnotQubits (Context (qC, qT)) (Context (pC, pT)) = do
  dC <- qC pC
  dT <- qT pT
  return $ cnotDecisions (Context (dC, dT))

-- Bell state preparation: H ⊗ I then CNOT
prepareBellPhiPlus :: Measurement
prepareBellPhiPlus props = do
  let q0 = qubit0
      hq = hadamard q0
      step1 = Context (hq, q0)
  cnotQubits step1 props

-- Main testing function
main :: IO ()
main = do
  putStrLn "Simulating Bell state preparation (|Φ+⟩) measured in Z ⊗ Z:"
  results <- replicateM 20 (prepareBellPhiPlus (Context (Z, Z)))
  mapM_ print results