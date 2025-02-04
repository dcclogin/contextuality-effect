module ExprRG where

import SyntaxRG
import State.ObservableRG
import State.ExperimentRG
import System.Random


-- generate a random ExprRG "instruction set" (R/G,R/G,R/G)
genExprRG :: Int -> ExprRG
genExprRG seed = (if b1 then R else G, if b2 then R else G, if b3 then R else G)
    where
        [b1, b2, b3] = take 3 (randomBoolStream seed)

-- random contexts combinations
ctx11 = [l1, r1]
ctx12 = [l1, r2]
ctx13 = [l1, r3]
ctx21 = [l2, r1]
ctx22 = [l2, r2]
ctx23 = [l2, r3]
ctx31 = [l3, r1]
ctx32 = [l3, r2]
ctx33 = [l3, r3]



printR :: IO ()
printR = do
    let expr = genExprRG 112 in
        do
            putStrLn $ "expr = " ++ show expr