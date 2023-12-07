import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace
import Data.Ord
import Data.List (group, sort)

data Rank = Joker | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
          | Jack | Queen | King | Ace deriving (Eq, Ord, Enum, Show)  

data Card = Card { rank :: Rank } deriving (Eq,Ord, Show)

data Grade = FiveOK | FourOK | FullHouse | ThreeOK | TwoPair | OnePair | HighCard deriving (Eq, Ord, Enum, Show)

data Hand = Hand {
    cards :: [Card],
    grade :: Grade,
    bid :: Int
} deriving (Eq, Show)

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        hands = sort $ map (\x -> readHand x rankParser) linesOfFiles 
        stonks = sum $ winnings (reverse hands) 1
        handsJoker = sort $ map (\x -> readHand x rankParserJoker) linesOfFiles 
        stonksJoker = sum $ winnings (reverse handsJoker) 1
    -- print hands
    print stonks
    -- print handsJoker
    print stonksJoker
    hClose handle

instance Ord Hand where
    compare left right 
        | grade left /= grade right = compare (grade left) (grade right)
        | otherwise = compare (cards right) (cards left)

rankParser :: Char -> Card
rankParser x
    | x == '2' = Card Two
    | x == '3' = Card Three
    | x == '4' = Card Four
    | x == '5' = Card Five
    | x == '6' = Card Six
    | x == '7' = Card Seven
    | x == '8' = Card Eight
    | x == '9' = Card Nine
    | x == 'T' = Card Ten
    | x == 'J' = Card Jack
    | x == 'Q' = Card Queen
    | x == 'K' = Card King
    | x == 'A' = Card Ace
    | otherwise = error "Dum dum"

rankParserJoker :: Char -> Card
rankParserJoker x
    | x == 'J' = Card Joker
    | otherwise = rankParser x

readHand :: String -> (Char -> Card) -> Hand
readHand string parser = Hand hnds (gradeCards hnds) (read bd)
    where 
        [crds, bd] = words string
        hnds = map parser crds

countOccurences :: Eq a => a -> [a] -> Int
countOccurences x = length . filter (x==)

mostFrequent :: [Card] -> Card
mostFrequent ns = x
    where x = snd (maximum [ (length ks, head ks) | ks <- group (sort ns) ])

evalGrades :: [Card] -> Int -> Grade
evalGrades crds mostJoker 
    | countMostFreq == 5 = FiveOK
    | countMostFreq == 4 = FourOK
    | countMostFreq == 3 && countScndFreq == 2 = FullHouse
    | countMostFreq == 3 = ThreeOK
    | countMostFreq == 2 && countScndFreq == 2 = TwoPair
    | countMostFreq == 2 = OnePair
    | otherwise = HighCard
    where 
        crdsWithoutJoker = (filter (/=Card Joker) crds)
        mostFreq = mostFrequent crdsWithoutJoker
        crdsWithoutJokerAndMostFreq = (filter (/=mostFreq) crdsWithoutJoker)
        scndFreq = mostFrequent crdsWithoutJokerAndMostFreq
        countMostFreq = countOccurences mostFreq crdsWithoutJoker + mostJoker
        countScndFreq = if crdsWithoutJokerAndMostFreq == [] then 0 else countOccurences scndFreq crdsWithoutJoker + countOccurences (Card Joker) crds - mostJoker

gradeCards :: [Card] -> Grade
gradeCards crds
    | countOccurences (Card Joker) crds == 5 = FiveOK
    | otherwise = x
    where 
        x = head $ y
        y = sort $ map (\x -> evalGrades crds x) [0..(countOccurences (Card Joker) crds)]

winnings :: [Hand] -> Int -> [Int]
winnings [] _ = []
winnings (hand:hands) iterator = iterator * (bid hand) : winnings hands (iterator + 1)