import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import System.IO
import System.Random

type WordSearchGrid = [[Char]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int) --column row
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

solveWordSearch :: [String] -> WordSearchGrid -> [(String,Maybe Placement)]
solveWordSearch _ [[]] = error "Empty Grid"                                              -- empty grid error
solveWordSearch [] _ = []                                                                -- no words no search returns empty
solveWordSearch wrds grid
  | checkValidGrid grid (length grid) == False = error "Invalid Grid"                    -- cakks checkValidGrid and if not valid(sizes mismach) calls an error
  | otherwise = filterHelper wrds filteredanswer []
      where rowcount = length grid
            filteredwords =  filter (not.null) wrds                                      -- Just in case if has any empty word string
            flatgr = concat grid                                                         -- flattening the grid to get posns of chars
            answer = checkAll grid rowcount (getAllPos flatgr filteredwords rowcount []) -- Gets all the [(String,Maybe Placement)] but has multiple leftover Nothing instances
            filteredanswer = filter ((/= Nothing).snd) answer                            -- removes all the extra Strings with Nothing instances to form a proper answer

checkValidGrid :: WordSearchGrid -> Int -> Bool                                          -- chargrid -> rowcount -> True/False if grid is valid
checkValidGrid [] _ = True                                                               -- if reaches end True
checkValidGrid (r:rs) rowcount
  | (length r == rowcount) = checkValidGrid rs rowcount
  | otherwise = False

charAtPosn :: WordSearchGrid -> Posn -> Char                                             -- finds element value at given position
charAtPosn grid (col,row) = (grid !! row) !! col

numbToPosn :: Int -> Int -> Posn                                                         -- calculates posn from given int number and size of grid 
numbToPosn n size = (n `mod` size,n `div` size)

getAllPos :: [Char] -> [String] -> Int -> [(String,Posn)] -> [(String,Posn)]             -- makes a list with all the possible word starting posns
getAllPos flatgrid [] rowcount acc = acc                                                 -- return acc after all words pass
getAllPos flatgrid (w:ws) rowcount acc = getAllPos flatgrid ws rowcount (acc++zipped)    -- grid 0:1.. words 0 0 rowcount acc[]
      where ch = w !! 0                                                                  -- first character of word
            chindexes = elemIndices ch flatgrid                                          -- all indexes of char in grid
            chposns = [numbToPosn x rowcount | x <- chindexes]                           -- converts all the indexes of the chars from flat grid to Posns
            zipped = zip (replicate (length chposns) w) chposns                          -- zips char posns with word

filterHelper :: [String] -> [(String,Maybe Placement)] -> [(String,Maybe Placement)] -> [(String,Maybe Placement)]  -- function for filtering extra Nothing in answer
filterHelper [] _ acc = acc                                                        -- return acc when passed through all words
filterHelper (w:ws) spss@(sp:sps) acc
  | w == fst sp = filterHelper ws sps (acc++sp:[])
  | otherwise = filterHelper ws spss (acc++(w,Nothing):[])

checkAll :: WordSearchGrid -> Int -> [(String,Posn)] -> [(String,Maybe Placement)] -- combines all possible "checkAllOrient" posns of chars for words into a list [(Word,Maybe Placement)]
checkAll grid maxsize wordposns = [checkAllOrient grid (snd pass) maxsize (fst pass) (fst pass) (snd pass) | pass <- wordposns]

checkAllOrient :: WordSearchGrid -> Posn -> Int -> String -> String -> Posn -> (String,Maybe Placement)   -- checking all possabilities for a Posn starting with a word's char
checkAllOrient grid (col,row) maxsize wrd@(c:cs) wordsave possave                                         -- as a plus we pass a save for the word and posn to use later
  | (col<maxsize && chkChar && (snd forward)/=Nothing) = forward                                          -- This down here checks for all possible moves from a posn and uses helper to continue
  | (col>=0 && chkChar && (snd back)/=Nothing) = back
  | (row>=0 && chkChar && (snd up)/=Nothing) = up
  | (row<maxsize && chkChar && (snd down)/=Nothing) = down
  | (row>=0 && col<maxsize && chkChar && (snd upforward)/=Nothing) = upforward
  | (row>=0 && col>=0 && chkChar && (snd upback)/=Nothing) = upback
  | (row<maxsize && col<maxsize && chkChar && (snd downforward)/=Nothing) = downforward
  | (row<maxsize && col>=0 && chkChar && (snd downback)/=Nothing) = downback
  | otherwise = (wordsave,Nothing)                                                                        -- returns (wordsave,Nothing) if no possible move
      where chkChar = c==(charAtPosn grid (col,row))                                                      -- checks if current char is possible on posn
            forward = checkAllOrientHelper grid (col+1,row) maxsize cs Forward wordsave possave           -- Forward move
            back = checkAllOrientHelper grid (col-1,row) maxsize cs Back wordsave possave                 -- Back move
            up = checkAllOrientHelper grid (col,row-1) maxsize cs Up wordsave possave                     -- Up move
            down = checkAllOrientHelper grid (col,row+1) maxsize cs Down wordsave possave                 -- Down move
            upforward = checkAllOrientHelper grid (col+1,row-1) maxsize cs UpForward wordsave possave     -- UpForward move
            upback = checkAllOrientHelper grid (col-1,row-1) maxsize cs UpBack wordsave possave           -- UpBack move
            downforward = checkAllOrientHelper grid (col+1,row+1) maxsize cs DownForward wordsave possave -- DownForward move
            downback = checkAllOrientHelper grid (col-1,row+1) maxsize cs DownBack wordsave possave       -- DownBack move

checkAllOrientHelper :: WordSearchGrid -> Posn -> Int -> String -> Orientation -> String -> Posn -> (String,Maybe Placement) -- Helper for checkAllOrient and goes to next step of given orientation
checkAllOrientHelper grid (col,row) maxsize [] orien wordsave possave = (wordsave,Just((possave,orien)))                     -- final if passes whole word gives Placement
checkAllOrientHelper grid (col,row) maxsize wrd@(c:cs) orien wordsave possave
  | (orien==Forward && col<maxsize && chkChar) = checkAllOrientHelper grid (col+1,row) maxsize cs orien wordsave possave -- Next Forward move
  | (orien==Back && col>=0 && chkChar) = checkAllOrientHelper grid (col-1,row) maxsize cs orien wordsave possave -- Next Back move
  | (orien==Up && row>=0 && chkChar) = checkAllOrientHelper grid (col,row-1) maxsize cs orien wordsave possave -- Next Up move
  | (orien==Down && row<maxsize && chkChar) = checkAllOrientHelper grid (col,row+1) maxsize cs orien wordsave possave -- Next Down move
  | (orien==UpForward && row>=0 && col<maxsize && chkChar) = checkAllOrientHelper grid (col+1,row-1) maxsize cs orien wordsave possave -- Next UpForward move
  | (orien==UpBack && row>=0 && col>=0 && chkChar) = checkAllOrientHelper grid (col-1,row-1) maxsize cs orien wordsave possave -- Next UpBack move
  | (orien==DownForward && row<maxsize && col<maxsize && chkChar) = checkAllOrientHelper grid (col+1,row+1) maxsize cs orien wordsave possave -- Next DownForward move
  | (orien==DownBack && row<maxsize && col>=0 && chkChar) = checkAllOrientHelper grid (col-1,row+1) maxsize cs orien wordsave possave -- Next DownBack move
  | otherwise = (wordsave,Nothing)
      where chkChar = c==(charAtPosn grid (col,row))                                                      -- checks if current char is possible on posn