import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        times = mapRead ( tail (words (linesOfFiles!!0)))
        distances = mapRead $ tail $ words (linesOfFiles!!1)
        result = foldr (*) 1 $ map length $ mapOptions distances times
        mergeTimes = mapRead [filter (/=' ') (unwords $ tail $ words (linesOfFiles!!0))]
        mergeDistances = mapRead [filter (/=' ') (unwords $ tail $ words (linesOfFiles!!1))]
        -- mergeResult = foldr (*) 1 $ map length $ mapOptions mergeDistances mergeTimes
        -- test = abcIntersect (mergeDistances!!0) (mergeTimes!!0) 1
        -- test2 = abcIntersectInv (mergeDistances!!0) (mergeTimes!!0) $ test + 1
        -- result2 = test2 - test
        (x1, x2) = functionRoot (-1) ((mergeTimes!!0)) (0-(mergeDistances!!0))
        check = (ceiling x2) - (ceiling x1)
    print times
    print distances
    print result
    print mergeTimes
    print mergeDistances
    -- print test
    -- print test2
    -- print result3
    print check
    hClose handle

mapRead :: [String] -> [Double]
mapRead [] = []
mapRead (string:strings) = read string : mapRead strings

timeForDistance :: Double -> Double -> Double
timeForDistance speed distance = distance / speed

fitsInTime :: Double -> Double -> Double -> Bool
fitsInTime distance record hold
    | timeForDistance hold distance < record = True
    | otherwise = False

options :: Double -> Double -> Double -> [Double]
options distance record hold
    | hold <= record && fitsInTime distance (record - hold) hold = (timeForDistance hold distance) + hold : options distance record (hold + 1)
    | hold <= record = options distance record (hold + 1)
    | otherwise = []

mapOptions :: [Double] -> [Double] -> [[Double]]
mapOptions [] _ = []
mapOptions _ [] = []
mapOptions (distance:distances) (record:records) = options distance record 0 : mapOptions distances records

abcIntersect :: Double -> Double -> Double -> Double
abcIntersect distance record holdTime 
    | timeTaken <= record = holdTime
    | otherwise = abcIntersect distance record $ holdTime + 1
    where
        timeTaken = holdTime + (timeForDistance holdTime distance)

abcIntersectInv :: Double -> Double -> Double -> Double
abcIntersectInv distance record holdTime 
    | timeTaken >= record = holdTime
    | otherwise = abcIntersectInv distance record $ holdTime + 1
    where
        timeTaken = holdTime + (timeForDistance holdTime distance)

functionRoot :: Double -> Double -> Double -> (Double, Double)
functionRoot a b c = if d < 0 then error "0" else (x, y)
    where
        x = e + sqrt d / (2 * a)
        y = e - sqrt d / (2 * a)
        d = b * b - 4 * a * c
        e = - b / (2 * a)