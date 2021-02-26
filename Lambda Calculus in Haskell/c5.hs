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

--------------PRETTY PRINTING---------------
prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef se expr) = (writeDefs se) ++ fst (checkContains expr se)

writeDefs :: [(String,LamExpr)] -> String -- writing out all the definitions as strings
writeDefs [] = ""
writeDefs [se] = "def " ++ fst se ++ " = " ++ (writeCheckedExpr (snd se) []) ++ " in "
writeDefs (se:ses) = "def " ++ fst se ++ " = " ++ (writeCheckedExpr (snd se) []) ++ " in " ++ writeDefs (ses)

checkContains :: LamExpr -> [(String,LamExpr)] -> (String,Bool) -- if expr is in definitions it writes it as the definition str, otherwise just evaluates it
checkContains expr@(LamMacro str) sel                           -- The bool is used to know if the expr has been converted to name from definitions
  | index == Nothing = error "Missing definition"               -- if macro string is not found it returns an error stating it
  | otherwise = (writeCheckedExpr expr sel,False)
     where index = findIndex ((str ==) . fst) sel
checkContains expr sel
  | index == Nothing = (writeCheckedExpr expr sel,False)
  | otherwise = (fst (sel!!indexInt),True)
     where index = findIndex ((expr ==) . snd) sel
           indexInt = extractIndex index

extractIndex :: Maybe Int -> Int                                -- takes the index from the maybe structure
extractIndex Nothing = -1
extractIndex (Just n) = n

writeCheckedExpr :: LamExpr -> [(String,LamExpr)] -> String     -- after expr is checked if it is contained in definitions, it is send here 
writeCheckedExpr ex@(LamMacro str) sel = str
writeCheckedExpr ex@(LamApp ex1 ex2) sel
  | bracketApp ex1 && snd check1 == False =  "(" ++ fst check1 ++ ") " ++ fst check2
  | otherwise = fst check1 ++ " " ++ fst check2
     where check1 = checkContains ex1 sel
           check2 = checkContains ex2 sel
writeCheckedExpr ex@(LamAbs i ex1) sel = "\\x" ++ show i ++ " -> " ++ fst check1
     where check1 = checkContains ex1 sel
writeCheckedExpr ex@(LamVar i) sel = "x" ++ show i

bracketApp :: LamExpr -> Bool                                   -- helps with establishing whether or not to add brackets  
bracketApp (LamMacro str) = False                               -- when dealing with lambda application
bracketApp (LamVar i) = False
bracketApp (LamApp ex1 ex2) = True
bracketApp (LamAbs i ex1) = True

-------------------------------------------------------------


prettyP :: LamMacroExpr -> String                                -- for testing with pretty printed output
prettyP ld@(LamDef se expr) = prettyPrint (cpsTransform ld)

-------------------------CH5--------------------------
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef sl ex) = LamDef newsl newex
     where replacedmacros = replaceMacros ex sl               -- Replaces starting expression parts with definition LamMacro if found
           defstartxs = findallVarDef sl []                   -- gets all definition variables in a list
           startexpxs = findallVar replacedmacros defstartxs  -- grows the list of variables with the expr variables
           newcpsdefs = cpsDefs sl startexpxs []              -- transforms the def to cps style, in a tuple with new variables list
           newxs = snd newcpsdefs                             -- takes the new variables list from defs
           newsl = fst newcpsdefs                             -- takes the new cps style defs
           defnewxs = findallVarDef newsl newxs               -- updates the variables list with then new cps defs
           newex = fst (transformExpr replacedmacros defnewxs)-- transforms the start expr to cps style

replaceMacros :: LamExpr -> [(String,LamExpr)] -> LamExpr -- Similar to checkContains in ch3 this functuon
replaceMacros expr@(LamMacro str) sel                     -- checks if a expr is contained in definitions
  | index == Nothing = error "Missing definition"         -- if it is in definitions it is replaced with LamMacro 
  | otherwise = expr
     where index = findIndex ((str ==) . fst) sel
replaceMacros expr@(LamAbs i ex1) sel
  | index == Nothing = LamAbs i (replaceMacros ex1 sel)
  | otherwise = LamMacro (fst (sel!!indexInt))
     where index = findIndex ((expr ==) . snd) sel
           indexInt = extractMacroIndex index
replaceMacros expr@(LamApp ex1 ex2) sel
  | index == Nothing = LamApp (replaceMacros ex1 sel) (replaceMacros ex2 sel)
  | otherwise = LamMacro (fst (sel!!indexInt))
     where index = findIndex ((expr ==) . snd) sel
           indexInt = extractMacroIndex index
replaceMacros expr@(LamVar i) sel
  | index == Nothing = expr
  | otherwise = LamMacro (fst (sel!!indexInt))
     where index = findIndex ((expr ==) . snd) sel
           indexInt = extractMacroIndex index

extractMacroIndex :: Maybe Int -> Int               -- Just like extractIndex in ch3 takes the index from the maybe structure
extractMacroIndex Nothing = -1                      -- added it just in case if separate testing
extractMacroIndex (Just n) = n

cpsDefs :: [(String,LamExpr)] -> [Int] -> [(String,LamExpr)] -> ([(String,LamExpr)],[Int]) -- Transforms the list of definitions to a cps style 
cpsDefs [] xs acc = (acc,xs)                                                               -- and keeps an accumulator of all claimed variables
cpsDefs [se] xs acc = (acc ++ (fst se,fst transf):[],xs)
     where transf = transformExpr (snd se) xs
cpsDefs (se:ses) xs acc = cpsDefs ses (snd transf) (acc ++ (fst se,fst transf):[])
     where transf = transformExpr (snd se) xs


transformExpr :: LamExpr -> [Int] -> (LamExpr,[Int]) -- main function for cps style transforming of an expression, given a list of used variables
transformExpr (LamMacro s) xs = (LamMacro s,xs)      -- it applies the 3 rules of cps on the lambda expr, keeping track of used variables on every step
transformExpr v@(LamVar i) xs = (expr,(new:i:[]++xs))
     where new = newVal xs
           expr = LamAbs new (LamApp (LamVar new) v) -- cps Rule
transformExpr (LamAbs i ex) xs = (expr,snd newex)
     where new = newVal xs
           newex = transformExpr ex (i:new:[]++xs)
           expr = LamAbs new (LamApp (LamVar new) (LamAbs i (fst newex))) -- cps Rule
transformExpr (LamApp ex1 ex2) xs = (expr,last)
     where new = newVal xs                                    -- gets a new value for a variable
           new2 = newVal (new:[] ++ xs)
           new3 = newVal (new2:new:[] ++ xs)
           newxs = (new3:new2:new:[] ++ xs)                   -- adds new variables to list
           newex1 = (transformExpr ex1 (newxs))               -- transforms the first expr in the application expression
           newex2 = (transformExpr ex2 (newxs++(snd newex1))) -- transforms the second expr in the application expression
           last = snd newex2                                  -- final list of used variables passed to the tuple in the result
           expr = LamAbs new (LamApp (fst newex1) (LamAbs new2 (LamApp (fst newex2) (LamAbs new3 (LamApp (LamApp (LamVar new2) (LamVar new3)) (LamVar new)))))) --cps Rule

findallVarDef :: [(String,LamExpr)] -> [Int] -> [Int] -- Similar to findallVar, finds all currently used variables in definition and adds them to an acc
findallVarDef [] acc = nub acc
findallVarDef [se] acc = nub ((findallVar (snd se) acc) ++ acc)
findallVarDef (se:ses) acc = findallVarDef ses newacc
      where newacc = nub ((findallVar (snd se) acc) ++ acc)

findallVar :: LamExpr -> [Int] -> [Int]  -- function to find all currently used variables in an expression and add them to an accumulator
findallVar (LamMacro s) acc = acc
findallVar (LamVar i) acc = (i:[] ++ acc)
findallVar (LamAbs i ex) acc = findallVar ex (i:[] ++ acc)
findallVar (LamApp ex1 ex2) acc = ((findallVar ex1 acc) ++ (findallVar ex2 acc) ++ acc)

newVal :: [Int] -> Int -- takes a list of ints and gives the next of size int not in list
newVal [] = 1
newVal xs = (maximum (nub xs)) + 1