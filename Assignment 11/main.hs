import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.List (group, sort, find, transpose)
import Data.Maybe
import Data.Map (Map, fromList, lookup)
import Prelude hiding (lookup)
import Prelude

data Galaxy = Galaxy {
    xCoord :: Integer,
    yCoord :: Integer,
    num :: Integer
} deriving (Eq, Show)

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let mapI = lines contents
        mapEx = transpose $ expandMapRows $ transpose $ expandMapRows mapI
        millRows = expandMapRowsMillion mapI 0
        millCols = flip expandMapRowsMillion 0 $ transpose mapI
        galaxies = constructGalaxies mapEx $ Galaxy 0 0 0
        galaxyDistances = flip div 2 $ sum $ map sum $ map (getGalaxyDistances galaxies) galaxies
        distantGalaxies = constructGalaxies mapI $ Galaxy 0 0 0
        galaxyDistancesM = flip div 2 $ sum $ map sum $ map (getGalaxyDistancesM distantGalaxies millRows millCols) distantGalaxies
    print galaxyDistances
    print galaxyDistancesM
    hClose handle

expandMapRows :: [[Char]] -> [[Char]]
expandMapRows [] = []
expandMapRows (row:rows) 
    | all (=='.') row = row : row : expandMapRows rows
    | otherwise = row : expandMapRows rows

expandMapRowsMillion :: [[Char]] -> Integer -> [Integer]
expandMapRowsMillion [] _ = []
expandMapRowsMillion (row:rows) int
    | all (=='.') row = int : expandMapRowsMillion rows (int + 1)
    | otherwise = expandMapRowsMillion rows (int + 1)

constructGalaxies :: [[Char]] -> Galaxy -> [Galaxy]
constructGalaxies [] galaxy = []
constructGalaxies ([]:ys) galaxy = constructGalaxies ys (galaxy {yCoord = yCoord galaxy + 1, xCoord = 0})
constructGalaxies ((x:xs):ys) galaxy 
    | x == '#' = galaxy : constructGalaxies ((xs):ys) (galaxy {xCoord = xCoord galaxy + 1, num = num galaxy + 1})
    | otherwise = constructGalaxies ((xs):ys) $ galaxy {xCoord = xCoord galaxy + 1}

getGalaxyDistance :: Galaxy -> Galaxy -> Integer
getGalaxyDistance galA galB = (abs (xCoord galA - xCoord galB)) + (abs (yCoord galA - yCoord galB))

getGalaxyDistances :: [Galaxy] -> Galaxy -> [Integer]
getGalaxyDistances [] _ = []
getGalaxyDistances (x:xs) galA = getGalaxyDistance x galA : getGalaxyDistances xs galA

constructList :: Integer -> Integer -> [Integer]
constructList x y
    | x > y = [y..x]
    | otherwise = [x..y]

getGalaxyDistanceM :: Galaxy -> [Integer] -> [Integer] -> Galaxy -> Integer
getGalaxyDistanceM galA rows cols galB = (abs (xCoord galA - xCoord galB)) + (abs (yCoord galA - yCoord galB)) + multRandC
    where
        crossedRows = toInteger $ length $ filter (\x -> elem x rows) $ constructList (yCoord galA) (yCoord galB)
        crossedCols = toInteger $ length $ filter (\x -> elem x cols) $ constructList (xCoord galA) (xCoord galB)
        crossAdd = toInteger $ 1000000 - 1
        multRandC = crossedCols * crossAdd + crossedRows * crossAdd

getGalaxyDistancesM :: [Galaxy] -> [Integer] -> [Integer] -> Galaxy -> [Integer]
getGalaxyDistancesM [] _ _ _ = []
getGalaxyDistancesM (x:xs) rows cols galA = getGalaxyDistanceM x rows cols galA : getGalaxyDistancesM xs rows cols galA
