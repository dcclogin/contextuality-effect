module RandomUtils where

import System.Random
import SyntaxRG

-- generate a random color R/G
genColor :: StdGen -> (RG, StdGen)
genColor gen =
    let (b, g) = random gen in
        if b then (R, g) else (G, g)

-- generate a random RGU R/G/undefined
genColorU :: StdGen -> (RGU, StdGen)
genColorU gen =
    let (n, g) = randomR (0 :: Int, 2) gen in
        case n of 
            0 -> (Nothing, g)
            1 -> (Just R, g)
            2 -> (Just G, g)
            _ -> error "impossible."

-- generate a random ExprRG "instruction set" (RG, RG, RG)
genExprRG :: StdGen -> (ExprRG, StdGen)
genExprRG gen =
    let (c1, g1) = genColor gen
        (c2, g2) = genColor g1
        (c3, g3) = genColor g2 
    in ((c1, c2, c3), g3)

-- generate a random ExprRGU (RGU, RGU, RGU)
genExprRGU :: StdGen -> (ExprRGU, StdGen)
genExprRGU gen =
    let (c1, g1) = genColorU gen
        (c2, g2) = genColorU g1
        (c3, g3) = genColorU g2 
    in ((c1, c2, c3), g3)

-- generate a random switch position S1/S2/S3
genPos :: StdGen -> (Position, StdGen)
genPos gen =
    let (n, g) = randomR (0 :: Int, 2) gen in
        case n of
            0 -> (S1, g)
            1 -> (S2, g)
            2 -> (S3, g)
            _ -> error "impossible."

-- generate a random configuration (a pair of switch positions)
genConfig :: StdGen -> (Config, StdGen)
genConfig gen =
    let (pos1, g1) = genPos gen
        (pos2, g2) = genPos g1
    in ((pos1, pos2), g2)

-- generate n random ExprRGs
genExprRGs :: Int -> StdGen -> [ExprRG]
genExprRGs 0 gen = []
genExprRGs n gen = let (e, g) = genExprRG gen in
    e : genExprRGs (n - 1) g

-- generate n random ExprRGUs
genExprRGUs :: Int -> StdGen -> [ExprRGU]
genExprRGUs 0 gen = []
genExprRGUs n gen = let (e, g) = genExprRGU gen in
    e : genExprRGUs (n - 1) g

-- generate n random configurations
genConfigs :: Int -> StdGen -> [Config]
genConfigs 0 gen = []
genConfigs n gen = let (c, g) = genConfig gen in
    c : genConfigs (n - 1) g