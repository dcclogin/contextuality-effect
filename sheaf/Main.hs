import FamousScenarios (ghz, merminSquare)
import LocalStateRunner (runWithLocalState)

main :: IO ()
main = do
  putStrLn "=== GHZ ==="
  runWithLocalState ghz
  putStrLn "\n=== Mermin Square ==="
  runWithLocalState merminSquare

  