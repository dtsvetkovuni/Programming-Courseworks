import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)

innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr -- inner reduction by one step 
innerRedn1 (LamDef se expr)
  | eval == expr = Nothing
  | otherwise = Just (LamDef se eval)
    where eval = eval1cbv expr se

outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr -- outer reduction by one step 
outerRedn1 (LamDef se expr)
  | eval == expr = Nothing
  | otherwise = Just (LamDef se eval)
    where eval = eval1cbn expr se

compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int,Maybe Int,Maybe Int,Maybe Int)
compareInnerOuter lme@(LamDef se expr) x
  | x<0 = (Nothing,Nothing,Nothing,Nothing)
  | otherwise = (answer1,answer2,answer3,answer4)
      where incps = formatcps lme            -- transforms expression for innercps to cps format
            outcps = formatcps lme           -- transforms expression for outercps to cps format
            evalin = evalcbv lme x           -- evaluates the definitions for each
            evalout = evalcbn lme x          --
            evalincps = evalcbv incps x      --
            evaloutcps = evalcbn outcps x    --
            answer1 = checkEvaluations evalin
            answer2 = checkEvaluations evalout
            answer3 = checkEvaluations evalincps
            answer4 = checkEvaluations evaloutcps

checkEvaluations :: (Maybe LamMacroExpr,Int) -> Maybe Int -- final function used to get the answer
checkEvaluations (Nothing,_) = Nothing
checkEvaluations ((Just evex),i)
  | checkFinnish evex = Just i
  | otherwise = Nothing


checkFinnish :: LamMacroExpr -> Bool -- checks if final macro has finnished all evaluations// Haven't covered all cases
checkFinnish (LamDef _ (LamVar _)) = True
checkFinnish (LamDef _ (LamMacro _)) = False
checkFinnish (LamDef _ (LamApp (LamAbs _ _) (LamVar _))) = True
checkFinnish (LamDef _ (LamApp (LamVar _) (LamAbs _ _))) = True
checkFinnish (LamDef _ (LamApp ex1 ex2)) = False
checkFinnish (LamDef _ (LamAbs _ ex1)) = True


formatcps :: LamMacroExpr -> LamMacroExpr -- runs the cps function from challange 5 and returns it applied to the ID expression
formatcps e@(LamDef se expr) = formated
    where (LamDef se' expr') = cpsTransform e
          formated = (LamDef se' (LamApp expr' exId))


reductions :: (LamMacroExpr -> Maybe LamMacroExpr) -> LamMacroExpr -> [(LamMacroExpr, LamMacroExpr)]  --adapted function from lecture 39
reductions ssev e = [ p | p <- zip evals (tail evals) ]
    where evals = iterate (\x -> let evex = ssev x in if evex == Nothing
                                                         then x
                                                         else removeJust evex) e

removeJust :: Maybe a -> a
removeJust (Just x) = x


eval :: (LamMacroExpr -> Maybe LamMacroExpr) -> Int -> Int -> [(LamMacroExpr, LamMacroExpr)] -> (Maybe LamMacroExpr,Int) --evaluates reductions and checks for limit counter
eval ssev count max reduct                                           -- outerRedn1 0 max (reductions outerRedn1)
  | fst (head reduct) == snd(head reduct) = (Just (fst (head reduct)),count)
  | count >= max = (Nothing,-1)
  | otherwise = eval ssev (count+1) max (drop 1 reduct)


evalcbn expr max= eval outerRedn1 0 max (reductions outerRedn1 expr) -- call by name evaluation function 
evalcbv expr max= eval innerRedn1 0 max (reductions innerRedn1 expr) -- call by value evaluation function 

--------LECTURE 38 Modified Functions----------
subst :: LamExpr -> Int -> LamExpr -> [ (String,LamExpr) ] -> LamExpr -- subst function addapted from lecture 38
subst (LamVar x) y e sle
  | x == y = e
  | x /= y = LamVar x
subst (LamAbs x e1) y e sle
  | x /= y && not (free x e) = LamAbs x (subst e1 y e sle)
  | x /=y && (free x e) = let x' = (rename x e1) in subst  (LamAbs x' (subst e1 x (LamVar x') sle)) y e sle
  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e sle = LamApp (subst e1 y e sle) (subst e2 y e sle)
subst (LamMacro str) y e sle = (snd (sle!!indexInt))  -- have to expand the str to the macro
      where index = findIndex ((str ==) . fst) sle
            indexInt = extractMacroIndex index

free :: Int -> LamExpr -> Bool -- used with the subst function addapted from lecture 38
free x (LamVar y) =  x == y 
free x (LamAbs y e) | x == y = False 
free x (LamAbs y e) | x /= y = free x e 
free x (LamApp e1 e2)  = (free x e1) || (free x e2)
free x (LamMacro str) = True

rename :: Int -> LamExpr -> Int -- used with the subst function addapted from lecture 38
rename x e
  | free (x+1) e = rename (x+1) e
  | otherwise = (x+1)

-- OUTERMOST
eval1cbn :: LamExpr -> [ (String,LamExpr) ] -> LamExpr 
eval1cbn (LamVar x) sle = (LamVar x)
eval1cbn (LamAbs x e) sle = (LamAbs x e)             -- not sure if I have to expand further into LamAbs
eval1cbn (LamApp (LamAbs x e1) e2) sle = subst e1 x e2 sle
eval1cbn (LamApp e1 e2) sle = LamApp (eval1cbn e1 sle) e2
eval1cbn (LamMacro str) sle = (snd (sle!!indexInt))  -- have to expand the str to the macro
      where index = findIndex ((str ==) . fst) sle
            indexInt = extractMacroIndex index

-- INNERMOST
eval1cbv :: LamExpr -> [ (String,LamExpr) ] -> LamExpr 
eval1cbv (LamVar x) sle = (LamVar x)
eval1cbv (LamAbs x e) sle = (LamAbs x e)             -- not sure if I have to expand further into LamAbs
eval1cbv (LamApp (LamAbs x e1) e@(LamAbs y e2)) sle = subst e1 x e sle
eval1cbv (LamApp e@(LamAbs x e1) e2) sle = LamApp e (eval1cbv e2 sle) 
eval1cbv (LamApp e1 e2) sle = LamApp (eval1cbv e1 sle) e2
eval1cbv (LamMacro str) sle = (snd (sle!!indexInt))  -- have to expand the str to the macro
      where index = findIndex ((str ==) . fst) sle
            indexInt = extractMacroIndex index
--------LECTURE 38 Modified Functions END----------
