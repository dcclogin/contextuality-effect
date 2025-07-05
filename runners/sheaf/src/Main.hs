
import FamousScenarios
import ContextualStateRunner

main :: IO ()
main = do
  putStrLn "=== GHZ ==="
  runWithContextualState ghz

  putStrLn "\n=== Mermin Square ==="
  runWithContextualState merminSquare

  putStrLn "\n=== Specker ==="
  runWithContextualState speckerTriangle

  putStrLn "\n=== Mermin 3 ==="
  runWithContextualState mermin3Qubit

  putStrLn "\n=== PRBox ==="
  runWithContextualState prBox
