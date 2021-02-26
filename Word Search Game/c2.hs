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

createWordSearch :: [String] -> Double -> IO WordSearchGrid
createWordSearch [] _ = return []                                         -- if no words returns []
createWordSearch words dens
  | dens <= 0 || dens > 1 = error "Invalid density"                       -- checking invalid density values
  | otherwise = do startg <- genRandChars uniquechar gridsize             -- first generates a grid of random chars
                   toaddwords <- getRandWords words gridrowcount [] words -- tuples with words' chars and Posns
                   let answ = combWordGrid startg toaddwords gridrowcount -- random grid chars replaced with words' chars at Posns
                   return answ                                            -- answer
      where charset = concat words
            bigtosmallword = sortBy (\wrd1 wrd2 -> compare (length wrd2) (length wrd1)) words --optimiziation by first strating with bigger words
            wordcharcount = length charset                                --length of all chars from words
            uniquechar = nub charset                                      -- list of all unique chars used later for random grid
            maxwordsize = length (bigtosmallword!!0)                      -- the maximal size of a word
            minmaxsize = max (maxwordsize*maxwordsize) (ceiling $ (fromIntegral wordcharcount)/dens) -- min size of grid chars
            gridrowcount = findGridSize minmaxsize 0                      -- finds the min possible NxN row/col count of grid
            gridsize = gridrowcount*gridrowcount                          -- final count of grid cells


--Useful small functions START--
posnToIndex :: Posn -> Int -> Int -- Converts posn given size of grid to linear index 
posnToIndex (col,row) n = n*row + col

insertAt :: [a] -> Int -> a -> [a] -- Given arr index element it inserts it at indexs pos in arr
insertAt xs n el = take n xs ++ [el] ++ drop (n + 1) xs

chunks :: Int -> [a] -> [[a]] -- Splits list into chunks of n (haven't imported library just in case for CW specifications)
chunks n xs = takeWhile (not.null) $ unfoldr (Just . splitAt n) xs

merge [] ys = ys -- Combines 2 lists , if first list is smaller list => faster time
merge (x:xs) ys = x:merge ys xs

removeAT :: Int -> [a] -> [a] -- Removes element at index
removeAT n xs = take n xs ++ drop (1 + n) xs
--Useful small functions END--

findGridSize :: Int -> Int -> Int -- Finds the right size of grid N rol/col
findGridSize minsize n
  | n*n<minsize = findGridSize minsize (n+1)
  | otherwise = n

combWordGrid :: [Char] -> [(Posn,Char)] -> Int -> WordSearchGrid -- Overrides randomly placed words over in the start grid of random chars 
combWordGrid chrs [] size = chunks size chrs
combWordGrid chrs (pc:pcs) size= combWordGrid update pcs size
      where index = posnToIndex (fst pc) size
            uchar = snd pc
            update = insertAt chrs index uchar

genRandChars :: [Char] -> Int -> IO [Char]  -- Used to get a number of random chars from charset of the words based on size given
genRandChars chars 0 = return []
genRandChars chars repeat = do 
    c <- (randomRIO (0, length chars - 1))
    cs <- genRandChars chars (repeat-1)
    return (chars!!c:cs)


getRandWords :: [String] -> Int -> [(Posn,Char)] -> [String] -> IO [(Posn,Char)] -- Master Function controlls following randomgen functions and can restart if failed
getRandWords [] size pcs wordssave = return pcs                                  -- after all words pass return their char positions
getRandWords wrds@(w:ws) size pcs wordssave= do 
    newpcs <- getRandPos w size (size*size) 0 pcs [] wrds                        -- next word pass
    if newpcs /= []                                                              -- checks if finding random Posn fails
       then getRandWords ws size newpcs wordssave                                -- continue with the rest of the words
       else getRandWords wordssave size [] wordssave                             -- if fails restart and try again

getRandPos :: String -> Int -> Int -> Int -> [(Posn,Char)] -> [Posn] -> [String] -> IO [(Posn,Char)] -- Called by getRandWords tries to finds a valid random postion,keeps track of
getRandPos w size grsize hpossize pcs hpos wrds = do                                                 -- checked postions in hpos(history)
    row <- (randomRIO (0, size-1))
    col <- (randomRIO (0, size-1))
    let pos = (col,row)                                                                -- get random posn for word
        pass = notElem pos hpos                                                        -- checks if we already tried posn
    if pass                                                                            -- if have not already checked pos 
       then do let getorien =  getOrient w pos size                                    -- get all possible orientations of word for pos
               if getorien /= []                                                       -- if there are possible orientations
                  then do ro <- getRandOrient w pos size pcs getorien hpos wrds        -- try wrinting on one of them
                          if ro /= []                                                  -- if it didnt fail
                             then return ro                                            -- return the positions with the chars
                             else getRandPos w size grsize (hpossize+1) pcs (pos:hpos) wrds -- otherwise get a new posn and add this posn to history of checked
                  else getRandPos w size grsize (hpossize+1) pcs (pos:hpos) wrds       -- error state must return back word
       else if hpossize >= grsize                                                      -- checks if history size reached full
               then return []                                                          -- if full then return failed []
               else getRandPos w size grsize (hpossize+1) pcs (pos:hpos) wrds          -- if history size not full try next random pos


getRandOrient :: String -> Posn -> Int -> [(Posn,Char)] -> [Orientation] -> [Posn] -> [String] -> IO [(Posn,Char)]  -- tries to write [(posn,char)] for a random pos of list
getRandOrient w pos size pcs oss@(o:os) hpos wrds = do 
    let left = length oss                                                                       --checks left orientations but this is always >0
    tryorien <- (randomRIO (0,left-1))                                                          -- orientation to try index
    let try = checkOrient pcs pos w (oss!!tryorien) []                                          --checks writing, [] if not possible ,[(Posn,Char)] if has
        removed = removeAT tryorien oss                                                         -- we have to remove the tried orientation from the list
    if try /= []                                                                                -- if the writing tried is possible
       then return try                                                                          -- then return it
       else if removed /= []                                                                    -- if it is not, check if there other left orientations
            then getRandOrient w pos size pcs removed hpos wrds                                 --if there are, try again to get a new rand orient and write to it
            else return []                                                                      -- if no other orient, return empty


checkOrient :: [(Posn,Char)] -> Posn -> String -> Orientation -> [(Posn,Char)] -> [(Posn,Char)] -- Helper for checkAllOrient and goes to next step of given orientation
checkOrient cont (col,row) [] orien acc = merge acc cont                                        -- final if passes whole word gives Placement
checkOrient cont p@(col,row) wrd@(c:cs) orien acc
  | (orien==Forward && chkChar) = checkOrient cont (col+1,row) cs orien ((p,c):acc)             -- Next Forward move
  | (orien==Back && chkChar) = checkOrient cont (col-1,row) cs orien ((p,c):acc)                -- Next Back move
  | (orien==Up && chkChar) = checkOrient cont (col,row-1) cs orien ((p,c):acc)                  -- Next Up move
  | (orien==Down && chkChar) = checkOrient cont (col,row+1) cs orien ((p,c):acc)                -- Next Down move
  | (orien==UpForward && chkChar) = checkOrient cont (col+1,row-1) cs orien ((p,c):acc)         -- Next UpForward move
  | (orien==UpBack && chkChar) = checkOrient cont (col-1,row-1) cs orien ((p,c):acc)            -- Next UpBack move
  | (orien==DownForward && chkChar) = checkOrient cont (col+1,row+1) cs orien ((p,c):acc)       -- Next DownForward move
  | (orien==DownBack && chkChar) = checkOrient cont (col-1,row+1) cs orien ((p,c):acc)          -- Next DownBack move
  | otherwise = []
      where chkChar = checkOrientContent cont (col,row) c                                       -- checks if current char is possible on posn

checkOrientContent :: [(Posn,Char)] -> Posn -> Char -> Bool  -- secondary helper function for checkOrient
checkOrientContent [] _ _ = True                             -- checks if current char is possible on posn
checkOrientContent pcs pos c
  | (check == Nothing || checkOrientContentHelper check c pcs) = True
  | otherwise = False
      where check = findIndex (\x -> fst x == pos) pcs

checkOrientContentHelper :: Maybe Int -> Char -> [(Posn,Char)] -> Bool  --helper function to return True/False if char can occupy posn and return Bool
checkOrientContentHelper Nothing _ _ = True
checkOrientContentHelper (Just ch) c pcs = (snd (pcs !! ch)) == c


getOrient :: String -> Posn -> Int -> [Orientation]                              -- This function checks possible word orientations in grid based on give Posn
getOrient word (col,row) gridsize = getOrientHelper 1 rowc1 rowc2 colc1 colc2 []
      where wsize = length word
            rowc1 = (gridsize - row) >= wsize                                    -- word can be written with going down
            rowc2 = (row + 1) >= wsize                                           -- word can be written with going up
            colc1 = (gridsize - col) >= wsize                                    --word can be written with going Forward
            colc2 = (col + 1) >= wsize                                           -- word can be written with going Back

getOrientHelper :: Int -> Bool -> Bool -> Bool -> Bool -> [Orientation]  -> [Orientation] --based on Bool inputs from getOrient, returns list of possible orientations
getOrientHelper n down up forward back acc
  | (n==1 && up)              = getOrientHelper 2 down up forward back (Up:acc)           -- if up is possible to write adds it
  | (n==2 && down)            = getOrientHelper 3 down up forward back (Down:acc)         -- if up is possible to write adds it
  | (n==3 && back)            = getOrientHelper 4 down up forward back (Back:acc)         -- if up is possible to write adds it
  | (n==4 && forward)         = getOrientHelper 5 down up forward back (Forward:acc)      -- if up is possible to write adds it
  | (n==5 && up && forward)   = getOrientHelper 6 down up forward back (UpForward:acc)    -- if up is possible to write adds it
  | (n==6 && up && back)      = getOrientHelper 7 down up forward back (UpBack:acc)       -- if up is possible to write adds it
  | (n==7 && down && forward) = getOrientHelper 8 down up forward back (DownForward:acc)  -- if up is possible to write adds it
  | (n==8 && down && back)    = getOrientHelper 9 down up forward back (DownBack:acc)     -- if up is possible to write adds it
  | n==9 = acc                                                                            -- return acc
  | otherwise = getOrientHelper (n+1) down up forward back acc                            -- next case if this not possible