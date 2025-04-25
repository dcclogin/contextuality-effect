module RandomUtils where

import System.Random
import SyntaxRG

-- presupposition: consecutively generated values are independent
-- in the end we can wire up all "randoms" with one input generator

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

-- generate a random ExprRG (RG, RG, RG) "instruction set"
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
genExprRGs :: Int -> StdGen -> ([ExprRG], StdGen)
genExprRGs 0 gen = ([], gen)
genExprRGs n gen = 
    let (e, g) = genExprRG gen in
        let (es, g') = genExprRGs (n - 1) g in
            (e:es, g')

-- generate n random ExprRGUs
genExprRGUs :: Int -> StdGen -> ([ExprRGU], StdGen)
genExprRGUs 0 gen = ([], gen)
genExprRGUs n gen = 
    let (e, g) = genExprRGU gen in
        let (es, g') = genExprRGUs (n - 1) g in
            (e:es, g')

-- generate n random configurations
genConfigs :: Int -> StdGen -> ([Config], StdGen)
genConfigs 0 gen = ([], gen)
genConfigs n gen = 
    let (c, g) = genConfig gen in
        let (cs, g') = genConfigs (n - 1) g in
            (c:cs, g')

-- generate True with probability n/m
genTrue :: Int -> Int -> StdGen -> (Bool, StdGen)
genTrue n m gen =
    let (x, g) = randomR (0 :: Int, m - 1) gen in
        if x < n then (True, g) else (False, g)

-- generate True with probability abs(cos(θ))
genTrueCos :: Double -> StdGen -> (Bool, StdGen)
genTrueCos theta gen =
    let (x, g) = randomR (0 :: Double, 1) gen in
        if x < abs (cos theta) then (True, g) else (False, g)

-- generate True with probability abs(sin(θ))
genTrueSin :: Double -> StdGen -> (Bool, StdGen)
genTrueSin theta gen =
    let (x, g) = randomR (0 :: Double, 1) gen in
        if x < abs (sin theta) then (True, g) else (False, g)

