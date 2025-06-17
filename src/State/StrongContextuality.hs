module State.StrongContextuality where

-- not quantum-realizable
-- modified from the Othing model

import Config
import Control.Monad.State.Lazy


type Pixel = Outcome
type HiddenVar = Maybe Pixel
type M = StateT HiddenVar IO


renderPixel :: Property -> M Pixel
renderPixel prop = do 
  dec <- liftIO randomDecision
  return $ Outcome prop dec


-- a set of rule enforcing the correlation
-- enforce :: Pixel -> Pixel