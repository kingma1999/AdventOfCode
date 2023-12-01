import System.IO  
import Control.Monad
import Data.Char

main = do  
    let list = []
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        score = sum (fAlNumber linesOfFiles)
    print score
    hClose handle

spelledDigit :: [Char] -> Int
spelledDigit x
    | (take 4 x) == "zero" = 0
    | (take 3 x) == "one" = 1
    | (take 3 x) == "two" = 2
    | (take 5 x) == "three" = 3
    | (take 4 x) == "four" = 4
    | (take 4 x) == "five" = 5
    | (take 3 x) == "six" = 6
    | (take 5 x) == "seven" = 7
    | (take 5 x) == "eight" = 8
    | (take 4 x) == "nine" = 9
    | reverse (take 4 x) == "zero" = 0
    | reverse (take 3 x) == "one" = 1
    | reverse (take 3 x) == "two" = 2
    | reverse (take 5 x) == "three" = 3
    | reverse (take 4 x) == "four" = 4
    | reverse (take 4 x) == "five" = 5
    | reverse (take 3 x) == "six" = 6
    | reverse (take 5 x) == "seven" = 7
    | reverse (take 5 x) == "eight" = 8
    | reverse (take 4 x) == "nine" = 9
    | otherwise = 11

isSpelledDigit :: [Char] -> Bool
isSpelledDigit input
    | (spelledDigit input) /= 11 = True
    | otherwise = False

firstNumber :: [Char] -> Int
firstNumber (x:xs)
    | (isDigit x) = (ord x) - (ord '0')
    | (isSpelledDigit (x:xs)) = spelledDigit (x:xs)
    | otherwise = firstNumber xs

lastNumber :: [Char] -> Int
lastNumber input
    | (isDigit (last input)) = (ord (last input)) - (ord '0')
    | isSpelledDigit (reverse input) = spelledDigit (reverse input)
    | otherwise = lastNumber (init input)

concatTwoIntToInt :: Int -> Int -> Int
concatTwoIntToInt n1 n2 = read ((show n1) ++ (show n2))

fAlNumber :: [String] -> [Int]
fAlNumber [] = []
fAlNumber (x:xs) = concatTwoIntToInt (firstNumber x) (lastNumber x) : fAlNumber xs
