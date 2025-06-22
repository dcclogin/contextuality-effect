module State.PaperCheating (sys2, run2) where

import Config
import Context2
import RandomUtils
import Control.Monad.State.Lazy


{--
Cheating model assumes
1. a classical source as local hidden variable, and
2. a communication channel as nonlocal one for cheating.
--}


type Pixel = (Property, Decision)
type HiddenVarC = Paper
type HiddenVarQ = Maybe Pixel
type HiddenVar = (HiddenVarC, HiddenVarQ)
type M = StateT HiddenVarQ IO
type Copy = Property -> M Decision


srcC :: IO HiddenVarC
srcC = randomPaper

srcQ :: IO HiddenVarQ
srcQ = return Nothing


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


renderPixel :: Property -> M Pixel
renderPixel prop = do d <- liftIO randomDecision; return (prop, d)


protocol :: Maybe Pixel -> Pixel -> Pixel -> M Decision
protocol py p1 (_, dec2) = case (py, p1) of
  (Nothing, (_, dec1)) -> do put (Just p1); return dec1
  (Just (propY, decY), (propM, dec1)) | propY == propM -> return decY
  (Just (propY, decY), (propM, dec1)) | decY == dec1 -> return dec2
  (Just (propY, decY), (propM, dec1)) | decY /= dec1 -> return dec1
  _ -> error "internal bug."


cp :: Paper -> Copy
cp paper = \prop -> do
  case getDecision paper prop of
    Just dec -> do
      let mine1 = (prop, dec)
      yours <- get
      mine2 <- renderPixel prop
      protocol yours mine1 mine2
    Nothing  -> error "internal bug."


makeCopy :: IO Paper -> IO (Context Copy)
makeCopy s = do paper <- s; return $ Context (cp paper, cp paper)


sys2 :: IO (Context Copy)
sys2 = makeCopy srcC


reifyEffect :: M (Context Decision) -> HiddenVarQ -> IO (Context Decision)
reifyEffect = evalStateT


-- hiding HiddenVar and export
run1 :: Copy -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- srcQ
  reifyEffect (traverse c ps) hvar


-- hiding HiddenVar and export
run2 :: Context Copy -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- srcQ
  reifyEffect (sequence $ cs <*> ps) hvar



{--
inspect :: HiddenVar -> (Copy, Copy) -> (Property, Property) -> IO (Decision, Decision)
inspect hvar (copy1, copy2) (prop1, prop2) =
	let m = (copy1 ⨷ copy2) (prop1, prop2) in 
		evalStateT m (snd hvar)


runTrial :: IO ReviewerAgreement
runTrial = do
  let r1 = Reviewer randomProperty
      r2 = Reviewer randomProperty
      pp = randomPaper
      tr = Trial {
          source = pp >>= (\paper -> return (paper, Nothing))
        , copies = makeCopy pp
        , reviewers = (r1, r2)
        , measure = inspect
      } 
  getAgreement $ executeTr tr


main :: IO ()
main = do
  printStats "(Cheating)" 11111 runTrial
--}


