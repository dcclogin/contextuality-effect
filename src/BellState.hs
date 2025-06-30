module BellState where

import Context2
import Control.Monad.State
import Control.Monad (replicateM)
import System.Random (randomIO)

-- Measurement properties (basis)
data Property = X | Y | Z deriving (Eq, Show)

-- Measurement outcomes
data Decision = Zero | One deriving (Eq, Show)

type M = StateT (Context Decision) IO

-- Flip a decision
flipDecision :: Decision -> Decision
flipDecision Zero = One
flipDecision One  = Zero

-- Uniformly random Zero or One
uniformDecision :: IO Decision
uniformDecision = do
  r <- randomIO
  return $ if r then Zero else One

-- Qubit is a function from Property to monadic effect
type Qubit m = Property -> m Decision

definiteIn :: (MonadIO m) => Property -> Decision -> Qubit m
definiteIn basis val p
  | p == basis = return val
  | otherwise  = liftIO uniformDecision

qubitZ0, qubitZ1 :: (MonadIO m) => Qubit m
qubitZ0 = definiteIn Z Zero
qubitZ1 = definiteIn Z One


-- Define Qubits that access shared BellState
initialQubits :: (MonadIO m) => Context (Qubit m)
initialQubits = Context (qubitZ0, qubitZ0)

-- Hadamard basis transform
hPerm :: Property -> Property
hPerm Z = X
hPerm X = Z
hPerm Y = Y

hadamard :: Monad m => Qubit m -> Qubit m
hadamard qubit p = qubit (hPerm p)


-- CNOT gate on Qubits using monadic effects
-- qc applied with a property, the result should be stored in shared state
-- CNOT gate on Qubits using shared state for control evaluation
cnotGate :: Context (Qubit M) -> Context (Qubit M)
cnotGate (Context (qc, qt)) = Context (qc', qt')
  where
    qc' prop = do
      val <- qc prop
      modify (\(Context (_, b)) -> Context (val, b))
      return val
    qt' prop = do
      Context (ctrl, _) <- get
      tgt <- qt prop
      return $ if ctrl == One then flipDecision tgt else tgt


-- Prepare Bell state: H ⊗ I then CNOT
bellState :: Context (Qubit M)
bellState = cnotGate (Context (hadamard qubitZ0, qubitZ0))


-- Sequence monadic effects to enforce nonlocal evaluation
measure :: Context (Qubit M) -> Context Property -> IO (Context Decision)
measure qs props = evalStateT (sequence $ qs <*> props) (Context (Zero, Zero))

-- Main testing function
main :: IO ()
main = do
  putStrLn "Simulating Bell state preparation (|Φ+⟩) measured in Z ⊗ Z:"
  results <- replicateM 20 $ measure bellState (Context (Z, Z))
  mapM_ print results

