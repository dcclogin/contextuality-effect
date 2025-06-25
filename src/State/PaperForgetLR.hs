module State.PaperForgetLR (sys2, run2, label) where

import Config
import Context2
import RandomUtils
import Control.Monad.State.Lazy


label :: String
label = "(State model -- Forget)"


-- probablistic contextuality: cannot have 3 determinate properties at the same time
-- strong contextuality: will lead to formal contradiction, global inconsistency

-- may be compared to Spekken's toy model and <knowledge-balance principle>


-- nonlocal hidden variable as state monad
type HiddenVar = (Paper, Paper)
type M = StateT HiddenVar IO


src :: IO HiddenVar
src = do paper <- randomPaper; return (paper, paper)


getDecision :: Either Property Property -> M (Maybe Decision)
getDecision prop = do
  (paper1, paper2) <- get
  case prop of
    Left Margins   -> return (margins paper1)
    Left FontSize  -> return (fontSize paper1)
    Left NumPages  -> return (numPages paper1)
    Right Margins  -> return (margins paper2)
    Right FontSize -> return (fontSize paper2)
    Right NumPages -> return (numPages paper2)

  
putDecision :: Either Property Property -> Maybe Decision -> M ()
putDecision prop d = do
  (paper1, paper2) <- get
  case prop of
    Left Margins  -> put (paper1 { margins = d }, paper2)
    Left FontSize -> put (paper1 { fontSize = d }, paper2)
    Left NumPages -> put (paper1 { numPages = d }, paper2)
    Right Margins  -> put (paper1, paper2 { margins = d })
    Right FontSize -> put (paper1, paper2 { fontSize = d })
    Right NumPages -> put (paper1, paper2 { numPages = d })


-- render a random decision for a specific property
renderDecision :: Either Property Property -> M Decision
renderDecision prop = do
  dd <- liftIO randomDecision
  putDecision prop (Just dd)
  return dd


-- unconditional forget
forgetDecision :: Either Property Property -> M ()
forgetDecision prop = putDecision prop Nothing


-- conditional forget
cforgetDecision :: Either Property Property -> (Maybe Decision -> Bool) -> M ()
cforgetDecision prop pred = do
  d <- getDecision prop
  if pred d then forgetDecision prop else return ()


-- forgetful-get
getDecisionF :: Either Property Property -> M (Maybe Decision)
getDecisionF (Left Margins) = do
  m <- getDecision (Left Margins)
  cforgetDecision (Left FontSize) (== m)
  cforgetDecision (Left NumPages) (== m)
  return m
getDecisionF (Left FontSize) = do
  m <- getDecision (Left FontSize)
  cforgetDecision (Left Margins) (== m)
  cforgetDecision (Left NumPages) (== m)
  return m
getDecisionF (Left NumPages) = do
  m <- getDecision (Left NumPages)
  cforgetDecision (Left Margins) (== m)
  cforgetDecision (Left FontSize) (== m)
  return m
getDecisionF (Right Margins) = do
  m <- getDecision (Right Margins)
  cforgetDecision (Right FontSize) (== m)
  cforgetDecision (Right NumPages) (== m)
  return m
getDecisionF (Right FontSize) = do
  m <- getDecision (Right FontSize)
  cforgetDecision (Right Margins) (== m)
  cforgetDecision (Right NumPages) (== m)
  return m
getDecisionF (Right NumPages) = do
  m <- getDecision (Right NumPages)
  cforgetDecision (Right Margins) (== m)
  cforgetDecision (Right FontSize) (== m)
  return m


-- the main logic for quantum system <appearance>
sys :: Either Property Property -> M Decision
sys prop = do
  d <- getDecisionF prop
  case d of
    Nothing -> renderDecision prop
    Just dd -> return dd


-- hiding the L/R information to reviewers
copy :: Direction -> Copy M
copy L p = sys (Left p)
copy R p = sys (Right p)


-- essentially causal
teleport :: Direction -> M ()
teleport L = do (paper1, _) <- get; put (paper1, paper1)
teleport R = do (_, paper2) <- get; put (paper2, paper2)


sys2 :: IO (Context (Copy M))
sys2 = return $ Context (copy L, copy R)


reifyEffect :: M (Context Decision) -> HiddenVar -> IO (Context Decision)
reifyEffect = evalStateT


-- hiding HiddenVar and export
{--
run1 :: Copy M -> Context Property -> IO (Context Decision)
run1 c ps = do
  hvar <- src
  reifyEffect (traverse c ps) hvar
--}


-- hiding HiddenVar and export
run2 :: Context (Copy M) -> Context Property -> IO (Context Decision)
run2 cs ps = do
  hvar <- src
  reifyEffect (sequence $ cs <*> ps) hvar


-- [TODO] connection to <call-by-reference> as in PL


-- this is an causal model
-- if we set the source to emit only (PPP), we can tell the causal flow


{--
-- no concept of quantum states and observables as projectors
-- is it still possible to express <commutativity>?
ltor, rtol :: Context Property -> IO HiddenVar -> IO HiddenVar
ltor = flip $ foldl f
  where
    f :: IO HiddenVar -> Property -> IO HiddenVar
    f s p = do hvar <- s; execStateT (sys p) hvar
rtol = flip $ foldr f
  where
    f :: Property -> IO HiddenVar -> IO HiddenVar
    f p s = do hvar <- s; execStateT (sys p) hvar
--}