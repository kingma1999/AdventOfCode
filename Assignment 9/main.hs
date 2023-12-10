import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.List (group, sort, find)
import Data.Maybe
import Data.Map (Map, fromList, lookup)
import Prelude hiding (lookup)
import Prelude

-- 2013357162

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        sensorList = map parseLine linesOfFiles
        pairDif = map expandSensor sensorList
        expandDown = map reverse $ map followDown $ map reverse pairDif
        score = sum $ map getNextValue expandDown
    print expandDown
    print score 
    hClose handle

parseLine :: String -> [[Integer]]
parseLine string = dat
    where
        dat = [map read $ reverse $ words string]

pairDifference :: [Integer] -> [Integer]
pairDifference (p1:p2:rest) = (p2 - p1) : pairDifference (p2:rest)
pairDifference (p1:[]) = []
pairDifference [] = error "Dum dum"

expandSensor :: [[Integer]] -> [[Integer]]
expandSensor (sensor:sensors)
    | all (==head sensor) (tail sensor) = [sensor]
    | otherwise = sensor : expandSensor [nextRow]
    where
        nextRow = pairDifference sensor

followDown :: [[Integer]] -> [[Integer]]
followDown (sensor1:sensor2:sensors) = (sensor1 : followDown (newSensor2:sensors))
    where
        newSensor2 = (sensor2 ++ ([last sensor1 + last sensor2]))
followDown (sensor1:_) = [sensor1]

getNextValue :: [[Integer]] -> Integer
getNextValue input = last $ head input
