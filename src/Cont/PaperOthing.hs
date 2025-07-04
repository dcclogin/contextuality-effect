module Cont.PaperOthing (
  bipartite, runfAll, runContextA, label
) where

import Config
import Context2
import RandomUtils
import PaperCore
import Contextuality
import Cont.EffectT
import Control.Monad.Cont
import Control.Concurrent.Async


label :: String
label = "(Continuation model -- Othing)"


-- Intuition: if thePaper is indeed nothing, then we don't really need a blueprint.
-- instead, we can just render a <partial paper>, or a <pixel> on-the-fly.

type M = YieldT Pixel Pixel Decision IO
-- [ DONE ] Judge as HiddenVar
-- [ OPTIONAL ] Judge as Reader Effect
type HiddenVar = Judge Context Pixel Pixel


src :: IO HiddenVar
src = return $ Judge $ \(Context (Susp pixel1 k1, Susp pixel2 k2)) -> do
  let proc1 = k1 pixel2 >>= \(Result d) -> return $ Context (d, snd pixel2)
      proc2 = k2 pixel1 >>= \(Result d) -> return $ Context (snd pixel1, d)
  race proc1 proc2 >>= \winner -> case winner of
    Left res  -> return res
    Right res -> return res


runContextA :: Context (Copy M) -> Context Property -> IO (Context Decision)
runContextA cs ps = src >>= \s -> runfAll s cs ps


protocol :: Pixel -> Pixel -> Pixel -> Decision
protocol (propY, decY) (propM, dec1) (_, dec2)
  | propY == propM = decY   -- Same property: take remote's value
  | decY == dec1   = dec2   -- Agreement: take secondary value
  | otherwise      = dec1   -- Disagreement: take primary value


copy :: Copy M
copy prop = do
  mine1 <- renderPixel prop -- primary rendering
  mine2 <- renderPixel prop -- secondary rendering
  yours <- yield mine1 -- receive yours; send mine1
  return (protocol yours mine1 mine2)


bipartite :: IO (Context (Copy M))
bipartite = distribute2 copy copy




-- [TODO] explore connection to <call-by-need> vs.<eager computation> as in PL
---- currently the concept <by-need> is not explicitly realized in semantics


-- [TODO] need to accommodate this model with interpretations in physics
---- candidate theory: retrocausality


-- in Mermin's experiment, the detector flashes instantly the moment the particle arrives
-- while here there is suspension and synchronisation happenining according to the semantics