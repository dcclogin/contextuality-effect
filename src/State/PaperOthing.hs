module State.PaperOthing where

import Config
import System.Random
import Control.Monad.State.Lazy


type Pixel = (Property, Decision)
type M = StateT Pixel IO


-- render an ad hoc decision for 
-- just one property|predicate|question|attribute|observable
renderPixel :: Property -> IO Pixel
renderPixel prop = do d <- randomDecision; return (prop, d)


-- [TODO] 
sys :: Property -> M Decision
sys prop = return Fail