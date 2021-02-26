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

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro s
  | null $ parsed = Nothing                    -- if left side is null returns Nothing
  | remain /= [] = Nothing                     -- checks if all is parsed
  | checkRepeated $ defs = Nothing             -- checks for repeated definitions
  | checkClosed defs [] [] == False = Nothing  -- checks if there are no free variables
  | otherwise = Just answer
     where parsed = parse macroExpr s
           answer = fst.head $ parsed
           remain = snd.head $ parsed
           defs = getDefs answer

getDefs :: LamMacroExpr -> [(String,LamExpr)]      -- extracts the definitions from LamMacroExpr
getDefs (LamDef sl _) = sl

checkRepeated :: [(String,LamExpr)] -> Bool        -- checks for repeated macro definition 
checkRepeated se = length (nub var) /= length var  -- compares size of unique names to actual names
     where var = concatMap (\(a, b) -> [a]) se

checkClosed :: [(String,LamExpr)] -> [Int] -> [String] -> Bool  -- checks for free variables
checkClosed [] defi defs = True
checkClosed (d:ds) defi defs
  | check = checkClosed ds defi ((fst d):[] ++ defs)
  | otherwise = False
     where check = checkClosedExpr (snd d) defi defs            -- calls checkClosedExpr to check if the expression is closed

checkClosedExpr :: LamExpr -> [Int] -> [String] -> Bool         -- for given expr evaluates and finds if it is closed in all cases
checkClosedExpr (LamMacro s) defi defs = True
checkClosedExpr (LamApp ex1 ex2) defi defs = checkClosedExpr ex1 defi defs && checkClosedExpr ex2 defi defs
checkClosedExpr (LamAbs i ex) defi defs = checkClosedExpr ex (i:[] ++ defi) defs
checkClosedExpr (LamVar i) defi defs = elem i defi

macroExpr :: Parser LamMacroExpr        -- main parser for the given LamMacroExpr
macroExpr = do defs <- some defLamExpr  -- evaluates the other parsers from the top "expr" function
               ex <- expr
               return (LamDef defs ex)
            <|> do ex2 <- expr          -- if there are no definitions try parsing expr
                   return (LamDef [] ex2)

defLamExpr ::  Parser (String,LamExpr)  -- parser for definitions in the LamMacroExpr
defLamExpr = do symbol "def"            -- parses a single definition at a time and returns it
                uch <- some upper
                symbol "="
                ex <- expr
                symbol "in"
                return (uch,ex)

macroLamExpr :: Parser LamExpr  -- parser for LamMacro expressions 
macroLamExpr = do space
                  uch <- some upper
                  return (LamMacro uch)

appLamExpr :: Parser LamExpr  -- parser for LamApp expressions
appLamExpr = do space
                ex1 <- some lvl1
                space
                return (foldl1 LamApp ex1)     -- idea to use foldl1 instead of a helper function - https://github.com/nikofil/LambdaInterpreter

absLamExpr :: Parser LamExpr  -- parser for LamAbs expressions
absLamExpr = do space
                string "\\x"
                dgts <- nat
                symbol "->"
                ex <- expr
                return (LamAbs dgts ex)

varLamExpr :: Parser LamExpr  -- parser for LamVar expressions
varLamExpr = do space
                char 'x'
                dgts <- nat
                return (LamVar dgts)

bracketExpr :: Parser LamExpr  -- parser for bracketed expressions
bracketExpr = do symbol "("
                 ex <- expr
                 symbol ")"
                 return ex

expr :: Parser LamExpr        -- main logic of parser and cases 
expr = appLamExpr <|> lvl1
lvl1 = absLamExpr <|> lvl2
lvl2 = bracketExpr <|> varLamExpr <|> macroLamExpr