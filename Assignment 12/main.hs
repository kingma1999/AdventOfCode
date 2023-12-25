import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.List (group, sort, find, transpose, delete, permutations)
import Data.Maybe
import Data.Map (Map, fromList, lookup)
import Prelude hiding (lookup)
import Prelude

-- 6412 low
-- 6294 low
-- 8412 high
-- 7965
-- 8239

-- 1 4 1 1 4 10 2 1 2 68 4

data Pixel = Black | White deriving (Eq)
data Row = Row {
    row :: [Maybe Pixel],
    spec :: [Int]
} deriving (Eq, Show)

instance Show Pixel where
    show Black = "#"
    show White = "."

main = do  
    handle <- openFile "inputTest.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        puzzle = map rowParser linesOfFiles
        testNum = 11
        possibility = rowPossesWrapper (puzzle!!testNum)
        possibilities = map rowPossesWrapper puzzle
        scores = calcScoreWrapper puzzle possibilities
        score = sum scores
        test = calcScore (puzzle!!testNum) $ possibility
        zeross = zeroFinder scores 0
    -- print puzzle
    -- print $ rowPoss (puzzle!!testNum) 0 0
    -- print possibility
    -- print test
    print possibilities
    print scores
    print score
    print zeross
    hClose handle

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

charParser :: Char -> Maybe Pixel
charParser x
    | x == '?' = Nothing
    | x == '.' = Just White
    | otherwise = Just Black

rowParser :: String -> Row
rowParser x = Row (preRet ++ [Just White]) $ map read $ splitOn "," $ input!!1
    where
        input = words x
        preRet = (map charParser (input!!0))

fitsEnd :: Row -> Bool 
fitsEnd x 
    | length (spec x) == 0 = True
    | length (row x) == 0 = False
    | row x !! 0 == Just Black && not specIsNotJW = False
    | checker = fitsEnd x {row = tail (row x)}
    | specIsNotJW = fitsEnd x {spec = tail (spec x), row = drop (((spec x)!!0) + 1) (row x)}
    | otherwise = fitsEnd x {row = tail (row x)}
    where
        specIsNotJW = all (/=Just White) (take ((spec x)!!0) (row x))
        checker = if length (row x) > ((spec x)!!0) then row x !! ((spec x)!!0) == Just Black else False

rowPoss :: Row -> Int -> Int -> [Int]
rowPoss x iterator depth
    | row x == [] = []
    | iterator > length (row x) - (length tailSpec) - sum (tailSpec) - 1 = []
    | sliceSize > iterator = 0 : rowPoss x (iterator + 1) depth
    | otherwise = y : rowPoss x (iterator + 1) depth
    where
        y = if allNotW && currentNotB && startNotB && nextFits && correctStart then 1 else 0
        sliceSize = ((spec x)!!0)
        tailSpec = tail (spec x)
        allNotW = all (/=Just White) (slice (iterator - sliceSize) (iterator - 1) (row x))
        currentNotB = (row x)!!iterator /= Just Black
        startNotB = if ((iterator - sliceSize - 1) >= 0) then ((row x)!!(iterator - sliceSize - 1) /= Just Black) else True
        nextFits = fitsEnd x {row = (drop (iterator + 1) (row x)), spec = tailSpec}
        helper = filter (not . null) (splitOn [Just White] (filter (/=Nothing) (take (iterator - sliceSize) (row x))))
        correctStart = depth >= length helper
        
toZero :: [Int] -> Bool -> [Int]
toZero [] _ = []
toZero (x:xs) passedDigit
    | x /= 0 || not passedDigit = x : toZero xs y
    | otherwise = []
    where
        y = if passedDigit || x /= 0 then True else False

rowAcc :: [Int] -> [Int] -> Int -> Int -> [Int]
rowAcc _ [] _ _ = []
rowAcc prev (x:xs) iterator spec = y : rowAcc prev xs (iterator + 1) spec
    where
        y = if x == 1 then sum takePart else 0
        takePart = (take (iterator - spec) prev)
        zeroStop = (toZero (reverse takePart) False)

rowPosses :: Row -> [Int] -> Int -> [Int]
rowPosses x posses depth
    | spec x == [] = posses
    | otherwise = rowPosses (x {spec = tail (spec x)}) newPoss (depth + 1)
    where
        newPoss = rowAcc posses (rowPoss x 0 depth) 0 ((spec x)!!depth)

rowPossesWrapper :: Row -> [Int]
rowPossesWrapper x = rowPosses (x {spec = tail (spec (x))}) (rowPoss x 0 0) 1

lastNum :: [Int] -> Int
lastNum x = head $ reverse $ filter (/=0) x

calcScore :: Row -> [Int] -> Int
calcScore x y = sum ( take ((firstB (reverse (row x)) 0) + 1) (reverse y))

calcScoreWrapper :: [Row] -> [[Int]] -> [Int]
calcScoreWrapper [] _ = []
calcScoreWrapper (x:xs) (y:ys) = calcScore x y : calcScoreWrapper xs ys

firstB :: [Maybe Pixel] -> Int -> Int
firstB [] forNow = forNow
firstB (x:xs) forNow
    | x == Just Black = forNow
    | otherwise = firstB xs (forNow + 1)

zeroFinder :: [Int] -> Int -> [Int]
zeroFinder [] _ = []
zeroFinder (x:xs) iterator
    | x == 0 = iterator : zeroFinder xs (iterator + 1)
    | otherwise = zeroFinder xs (iterator + 1)