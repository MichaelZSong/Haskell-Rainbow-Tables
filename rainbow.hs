import RainbowAssign
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map

-- import Debug.Trace (trace)

pwLength, nLetters, width, height :: Int
filename :: FilePath
pwLength = 8            -- length of each password
nLetters = 5            -- number of letters to use in passwords: 5 -> a-e
width = 40              -- length of each chain in the table
height = 1000           -- number of "rows" in the table
filename = "table.txt"  -- filename to store the table

-- Convert a hash back to a possible password
pwReduce :: Hash -> Passwd
pwReduce hash = reverse (take pwLength (map toLetter (convertToBase (fromEnum hash))))
-- Retrieved from: http://zvon.org/other/haskell/Outputprelude/map_f.html
-- Retrieved from: http://zvon.org/other/haskell/Outputprelude/take_f.html
-- Retrieved from: http://zvon.org/other/haskell/Outputprelude/reverse_f.html

-- Convert number to a list of base nLetters recursively
convertToBase :: Int -> [Int]
convertToBase num = remainder : convertToBase quotient
  where
    quotient = num `div` nLetters   -- Divide number by nLetters
    remainder = num `mod` nLetters  -- Take modulo of number with nLetters

-- Map the final hash to the password values
rainbowTable :: Int -> [Passwd] -> Map.Map Hash Passwd
rainbowTable _ [] = Map.empty
rainbowTable wid (pw : passwords) = Map.insert finalHash pw pwTable
  where
    initialHash = pwHash pw                   -- Initial hash of password
    finalHash = reduceChain wid initialHash   -- Final hash after hash/reduce operation
    pwTable = rainbowTable wid passwords      -- Build the table
-- Retrieved from: https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

-- Hash/reduce the initial hash recursively
reduceChain :: Int -> Hash -> Hash
reduceChain 0 hash = hash
reduceChain wid hash = reduceChain (wid - 1) (pwHash (pwReduce (fromIntegral hash)))

-- Reverse the hash function to find the corresponding password
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table wid targetHash = searchTable (Map.toList table) wid targetHash
-- Retrieved from: https://hackage.haskell.org/package/containers-0.4.0.0/docs/Data-Map.html

-- Iterate the table
searchTable :: [(Hash, Passwd)] -> Int -> Hash -> Maybe Passwd
searchTable [] _ _ = Nothing
searchTable ((aHash, aPasswd) : nextPairs) wid targetHash =
  case searchChain targetHash aHash aPasswd wid of
    Just result -> Just result
    Nothing -> searchTable nextPairs (wid + 1) targetHash

-- Iterate the chain to find corresponding password
searchChain :: Hash -> Hash -> Passwd -> Int -> Maybe Passwd
searchChain _ _ _ 0 = Nothing
searchChain targetHash aHash aPasswd remainWid
  | targetHash == pwHash aPasswd = Just aPasswd
  | otherwise = searchChain targetHash aHash (pwReduce (pwHash aPasswd)) (remainWid - 1)

-- Debug: trace ("Checking hash: " ++ show hash ++ ", Target hash: " ++ show targetHash) $ 

{-
findPassword :: Map.Map Hash Passwd -> Int -> Hash -> Maybe Passwd
findPassword table wid targetHash = 
  case concatMap (findPasswordChain targetHash wid) (Map.elems table) of
    [] -> Nothing
    (foundPasswd : _) -> Just foundPasswd
-- Retrieved from: http://zvon.org/other/haskell/Outputprelude/concatMap_f.html

findPasswordChain :: Hash -> Int -> Passwd -> [Passwd]
findPasswordChain _ 0 _ = []
findPasswordChain hash remainWid potentialPasswd
  | hash == pwHash potentialPasswd = [potentialPasswd]
  | otherwise = findPasswordChain hash (remainWid - 1) (pwReduce (fromIntegral (pwHash potentialPasswd)))
-}

-- TESTING FUNCTIONS
-- Generate a new table and save it to disk
generateTable :: IO ()
generateTable = do
  table <- buildTable rainbowTable nLetters pwLength width height
  writeTable table filename

-- Test the expression in the generated table file
test1 :: IO (Maybe Passwd)
test1 = do
  table <- readTable filename
  return (Map.lookup 0 table)   -- Replace 0 with hash to test

test2 :: Int -> IO ([Passwd], Int)
test2 n = do
  table <- readTable filename
  pws <- randomPasswords nLetters pwLength n
  let hs = map pwHash pws
  let result = Maybe.mapMaybe (findPassword table width) hs
  return (result, length result)
