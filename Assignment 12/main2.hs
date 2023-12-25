import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.List (group, sort, find, transpose, delete, permutations, findIndices)
import Data.Maybe
import Data.Map (Map, fromList, lookup)
import Prelude hiding (lookup)
import Prelude

data Pixel = Black | White deriving (Eq)
data Row = Row {
    row :: [Maybe Pixel],
    spec :: [Int]
} deriving (Eq, Show)

instance Show Pixel where
    show Black = "#"
    show White = "."


-- [1,4,1,1,4,10,2,1,2,68,4,13,9]

pprint :: (Show a) => [a] -> String
pprint [] = []
pprint (x:xs) = show x ++ ('\n' : pprint xs)
 
main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        puzzle = map rowParser linesOfFiles
        puzzle2 = map rowReParser puzzle
        meltedFunction = (\x -> scoreProcessor x (scorerWrapper (possPlacesItr 0 x) x 1))
        test1 = sum $ map meltedFunction puzzle
        test2 = sum $ map meltedFunction puzzle2
        y = puzzle2!!0
        helper = possPlacesItr 0 y
        helper2 = (scorerWrapper helper y 1)
        helper3 = scoreProcessor y helper2
    -- print puzzle
    -- print puzzle2
    print test1
    print test2
    -- putStrLn $ pprint helper
    -- print helper2
    -- print helper3
    hClose handle

rowReParser :: Row -> Row
rowReParser x = Row rowsBetter y 
    where
        rows = concat (init (row x) : replicate 4 (Nothing : init (row x)))
        rowsBetter = rows ++ [Just White]
        y = concat (replicate 5 (spec x))

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

countSepHash :: [Maybe Pixel] -> Bool -> Int -> Int
countSepHash x prevHash counter = (length y)
    where
        y = filter (/=[]) $ splitOn [Just White] $ filter (/=Nothing) x

possiblePlaces :: Int -> Int -> Row -> [Int]
possiblePlaces iterator depth x
    | iterator >= length (row x) = []
    | otherwise = y : possiblePlaces (iterator + 1) depth x 
    where
        y = if correctBefore && partNotW && currentNotB then 1 else 0
        len = (spec x)!!depth
        partNotW = if iterator - len >= 0 then all (/=Just White) (slice (iterator - len) (iterator - 1) (row x)) else False
        amountOfBBefore = countSepHash (take (iterator - len) (row x)) False 0
        currentNotB = (row x)!!(iterator) /= Just Black
        correctBefore = amountOfBBefore <= depth 

possPlacesItr :: Int -> Row -> [[Int]]
possPlacesItr depth x
    | depth >= length (spec x) = []
    | otherwise = possiblePlaces 0 depth x : possPlacesItr (depth + 1) x

scorerHelper :: Row -> [Int] -> [Int]
scorerHelper x y
    | indices == [] = y
    | otherwise = z 
    where
        z = if untilHB == [] then y else drop ((last untilHB)) y
        rowLeft = take (length y) (row x)
        indices = findIndices (==Just Black) rowLeft
        highB = last indices
        untilHB = findIndices (==0) (take (highB + 1) y)

scorer :: [Int] -> Int -> Int -> [Int] -> Row -> [Int]
scorer prev iterator depth curr z
    | iterator + 1 > length curr = []
    | otherwise = (if nonZero then y else 0) : scorer prev (iterator + 1) depth curr z
    where
        nonZero = curr!!iterator /= 0
        yy = (take (iterator - ((spec z)!!depth)) prev)
        y = sum (scorerHelper z yy)

scorerWrapper :: [[Int]] -> Row -> Int -> [Int]
scorerWrapper (x1:x2:xs) y depth
    | xs == [] = run
    | otherwise = scorerWrapper (run:xs) y (depth + 1)
    where
        run = scorer x1 0 depth x2 y

scoreProcessor :: Row -> [Int] -> Int
scoreProcessor x score = sum $ drop validAfter score
    where
        indices = findIndices (==Just Black) (row x)
        validAfter = if length indices > 0 then (last indices) + 1 else 0