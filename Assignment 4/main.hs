import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace

data Card = Card { 
    cardNum :: Int,
    winningNums :: [Int],
    yourNums :: [Int],
    copies :: Int,
    count :: Int
} deriving Show

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        cards = map parseCard linesOfFiles
        scores = pointsWrapper cards
        score = sum scores
        -- test = cardsProcessor cards 0 cards
        test = scratchProcessor (reverse cards) [0]
        result = (sum (map (+1) test)) - 1
    print cards
    print score
    print test
    print result
    hClose handle

parseCard :: String -> Card
parseCard input = Card cardNum winningNums yourNums (scratchCopiesWrapper (Card cardNum winningNums yourNums 0 0)) 0
    where
        cardSplit = splitOn ":" input
        cardNum = read ((words (cardSplit!!0))!!1)
        numsSplit = splitOn "|" (cardSplit!!1)
        winningNums = map read (words (numsSplit!!0))
        yourNums = map read (words (numsSplit!!1))

isDouble :: [Int] -> Int -> Bool
isDouble [] _ = False
isDouble (ref:refs) check 
    | ref == check = True
    | otherwise = isDouble refs check

points :: [Int] -> [Int] -> Int -> Int
points ref [] currentPoints = currentPoints
points ref (num:nums) currentPoints
    | isDouble ref num && currentPoints == 0 = points ref nums 1
    | isDouble ref num && currentPoints /= 0 = points ref nums (currentPoints * 2)
    | otherwise = points ref nums currentPoints

scratchCopies :: [Int] -> [Int] -> Int -> Int
scratchCopies ref [] currentPoints = currentPoints
scratchCopies ref (num:nums) currentPoints
    | isDouble ref num = scratchCopies ref nums (currentPoints + 1)
    | otherwise = scratchCopies ref nums currentPoints

scratchCopiesWrapper :: Card -> Int
scratchCopiesWrapper card = scratchCopies (winningNums card) (yourNums card) 0

pointsWrapper :: [Card] -> [Int]
pointsWrapper [] = []
pointsWrapper (x:xs) = points (winningNums x) (yourNums x) 0 : pointsWrapper xs

queueGenerator :: Card -> [Int]
queueGenerator card = [((cardNum card) + 1) .. (scratchCopiesWrapper card)]

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

cardsRefAccessor :: [Card] -> Int -> Int -> [Card] 
cardsRefAccessor ref low high 
    | 0 <= low && low < (length ref) && 0 <= high && high < (length ref) = slice low high ref
    | 0 <= low && low < (length ref) && 0 <= high && high >= (length ref) = slice low (length ref) ref
    | low == high && low < (length ref) = [(ref!!low)]
    | otherwise = ([])
    -- | otherwise = trace ((show low) ++ " : " ++ (show high)) (error "Dum dum")

cardsProcessor :: [Card] -> Int -> [Card] -> Int
cardsProcessor [] counter _ = counter
cardsProcessor (q:ques) counter ref = (cardsProcessor (ques ++ (cardsRefAccessor ref low high)) (counter + 1) ref)
    where 
        low = ((cardNum q))
        high = (cardNum q) + (scratchCopiesWrapper q) - 1

scratchRefAccessor :: [Int] -> Int -> Int -> Int
scratchRefAccessor ref low high 
    | 0 <= low && low < (length ref) && 0 <= high && high < (length ref) = sum (slice low high ref)
    | 0 <= low && low < (length ref) && 0 <= high && high >= (length ref) = sum (slice low (length ref) ref)
    | low == high && low < (length ref) = (ref!!low)
    | otherwise = 0

scratchProcessor :: [Card] -> [Int] -> [Int]
scratchProcessor [] counts = counts
scratchProcessor (card:cards) counts = (scratchProcessor cards ((scratchRefAccessor counts low high) + (copies card) : counts))
    where 
        low = 0
        high = (copies card) - 1