module State.PaperCheating where

import Config
import System.Random
import Control.Monad.State.Lazy


{--
Cheating model assumes
1. a classical source as local hidden variable, and
2. a communication channel as nonlocal one for cheating.
--}


type Pixel = (Property, Decision)
type HiddenVarC = Paper
type HiddenVarQ = Maybe Pixel
type M = StateT HiddenVarQ IO
type HiddenVar = (HiddenVarC, HiddenVarQ)


getDecision :: Paper -> Property -> Maybe Decision
getDecision paper prop = case prop of
  Margins   -> margins paper
  FontSize  -> fontSize paper
  NumPages  -> numPages paper


renderPixel :: Property -> M Pixel
renderPixel prop = do d <- liftIO randomDecision; return (prop, d)


type Copy = Property -> M Decision


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


-- bipartite system
(⨷) :: (Property -> M Decision) 
    -> (Property -> M Decision) 
    -> (Property, Property) -> M (Decision, Decision)
(sys1 ⨷ sys2) (prop1, prop2) = do
  d1 <- sys1 prop1
  d2 <- sys2 prop2
  return (d1, d2)


makeCopy :: IO Paper -> IO (Copy, Copy)
makeCopy sc = do paper <- sc; return (cp paper, cp paper)


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


