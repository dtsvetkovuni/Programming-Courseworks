import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

data LamMacroExpr = LamDef [(String,LamExpr)] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)


prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef se expr) = (writeDefs se se) ++ fst (checkContains expr se)

writeDefs :: [(String,LamExpr)] -> [(String,LamExpr)] -> String -- writing out all the definitions as strings
writeDefs [] acc = ""
writeDefs [se] acc = "def " ++ fst se ++ " = " ++ (writeCheckedExpr (snd se) acc) ++ " in "
writeDefs (se:ses) acc = "def " ++ fst se ++ " = " ++ (writeCheckedExpr (snd se) acc) ++ " in " ++ writeDefs ses (se:acc)

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
  | fstBracketApp ex1 && snd check1 && sndBracketApp ex2 && snd check2  == False = "(" ++ fst check1 ++ ") (" ++ fst check2 ++ ")"
  | fstBracketApp ex1 && snd check1 == False = "(" ++ fst check1 ++ ") " ++ fst check2
  | sndBracketApp ex2 && snd check2 == False = fst check1 ++ " (" ++ fst check2 ++ ")"
  | otherwise = fst check1 ++ " " ++ fst check2
     where check1 = checkContains ex1 sel
           check2 = checkContains ex2 sel
writeCheckedExpr ex@(LamAbs i ex1) sel = "\\x" ++ show i ++ " -> " ++ fst check1
     where check1 = checkContains ex1 sel
writeCheckedExpr ex@(LamVar i) sel = "x" ++ show i

fstBracketApp :: LamExpr -> Bool                                   -- helps with establishing whether or not to add brackets  
fstBracketApp (LamMacro str) = False                               -- when dealing with lambda application fst expression
fstBracketApp (LamVar i) = False
fstBracketApp (LamApp ex1 ex2) = fstBracketApp ex2
fstBracketApp (LamAbs i ex1) = True

sndBracketApp :: LamExpr -> Bool                                   -- helps with establishing whether or not to add brackets  
sndBracketApp (LamMacro str) = False                               -- when dealing with lambda application's snd expression
sndBracketApp (LamVar i) = False
sndBracketApp (LamApp ex1 ex2) = True
sndBracketApp (LamAbs i ex1) = False