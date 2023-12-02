import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace

data Throw = Throw { red :: Int
                    , green :: Int
                    , blue :: Int
                    } deriving Show

data Game = Game { number :: Int
                    , valid :: Bool
                    , throws :: [Throw]
                    } deriving Show

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        games = delimGames linesOfFiles
        validGames = validator games (Throw 12 13 14)
        score = sum (validGameIDs validGames)
        throwsNeeded = minThrowPerGame games
    print score
    print (throwsSum throwsNeeded)
    hClose handle

spelledColor :: [Char] -> Int -> Throw -> Throw
spelledColor x num input
    | (take 3 x) == "red" = input {red = num}
    | (take 5 x) == "green" = input {green = num}
    | (take 4 x) == "blue" = input {blue = num}
    | otherwise = trace ("Input: " ++ x) (error "Dum dum")

concatTwoIntToInt :: Int -> Int -> Int
concatTwoIntToInt n1 n2 = read ((show n1) ++ (show n2))

delimRGB :: [String] -> Throw -> Throw
delimRGB [] _ = Throw 0 0 0 
delimRGB (value:param:xs) throw = spelledColor param (read value) (delimRGB xs throw)

delimThrows :: [String] -> [Throw]
delimThrows [] = []
delimThrows (x:xs) = (delimRGB (words x) (Throw 0 0 0)) : delimThrows xs

delimGame :: [String] -> Game
delimGame (x:xs) = Game (read ((words x)!!1)) False (delimThrows (splitOn ";" (xs!!0)))

delimGames :: [String] -> [Game]
delimGames [] = []
delimGames (x:xs) = delimGame (splitOn ":" x) : delimGames xs

allLheqThanR :: Throw -> Throw -> Bool
allLheqThanR l r
    | (red l) >= (red r) && (green l) >= (green r) && (blue l) >= (blue r) = True
    | otherwise = False

allThrowsLEQReq :: [Throw] -> Throw -> Bool
allThrowsLEQReq [] _ = True
allThrowsLEQReq (throw:throws) req
    | allLheqThanR req throw = allThrowsLEQReq throws req
    | otherwise = False

validatorGame :: Game -> Throw -> Game
validatorGame game req 
    | allThrowsLEQReq (throws game) req = game {valid = True}
    | otherwise = (game {valid = False})

validator :: [Game] -> Throw -> [Game]
validator [] _ = []
validator (game:games) req = validatorGame game req : validator games req

validGameIDs :: [Game] -> [Int]
validGameIDs [] = []
validGameIDs (game:games) 
    | (valid game) = (number game) : validGameIDs games
    | otherwise = validGameIDs games

largestValue :: Int -> Int -> Int
largestValue x1 x2
    | x1 > x2 = x1
    | otherwise = x2

minThrowNeededForThrows :: [Throw] -> Throw -> Throw
minThrowNeededForThrows [] highestNow = highestNow
minThrowNeededForThrows (throw:throws) highestNow = minThrowNeededForThrows throws (Throw r g b)
        where
            r = largestValue (red throw) (red highestNow)
            g = largestValue (green throw) (green highestNow)
            b = largestValue (blue throw) (blue highestNow)
            

minThrowNeededForGame :: Game -> Throw
minThrowNeededForGame game = minThrowNeededForThrows (throws game) (Throw 0 0 0)

minThrowPerGame :: [Game] -> [Throw]
minThrowPerGame [] = []
minThrowPerGame (game:games) = minThrowNeededForGame game : minThrowPerGame games

throwsSum :: [Throw] -> Int
throwsSum [] = 0
throwsSum (throw:throws) = (red throw) * (green throw) * (blue throw) + (throwsSum throws)

