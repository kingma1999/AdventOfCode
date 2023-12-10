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

data Coord = Coord {
    xCoord :: Int,
    yCoord :: Int
} deriving (Show, Eq)

data Dir = North | East | South | West deriving (Enum, Show, Eq)

data Dijkstra = Dijkstra {
    step :: Int,
    dir :: Dir,
    coord :: Coord,
    passed :: [Char]
} deriving (Eq, Show)

-- instance Num Dir where
--     (+) a b = mod (a + (b)) 4

-- (+) :: (Dir a, Int b) => Dir a -> Int b -> Dir char 
-- a + b = toEnum $ mod ((fromEnum a) + b) 4


main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let map = lines contents
        coordS = findS map (Coord 0 0)
        (finDijk, maps) = dijkstraIter map (Dijkstra 0 North coordS [])
    print coordS
    print finDijk
    print maps
    print $ div (step finDijk) 2
    hClose handle

(<+>) :: Dir -> Int -> Dir
(<+>) a b = toEnum $ mod ((fromEnum a) + b) 4

coordToChar :: Coord -> [[Char]] -> Char
coordToChar coord map = ((map!!(yCoord coord))!!(xCoord coord))

dirToCoord :: Dijkstra -> [[Char]] -> Coord
dirToCoord dijkstra map 
    | dir dijkstra == North = ((coord dijkstra) {yCoord = yCoord (coord dijkstra) - 1})
    | dir dijkstra == East = ((coord dijkstra) {xCoord = xCoord (coord dijkstra) + 1})
    | dir dijkstra == South = ((coord dijkstra) {yCoord = yCoord (coord dijkstra) + 1})
    | dir dijkstra == West = ((coord dijkstra) {xCoord = xCoord (coord dijkstra) - 1})

nextDir :: Dir -> Coord -> [[Char]] -> Dir
nextDir dir coord map
    | (coordToChar coord map) == 'S' = North
    | dir == North = case (coordToChar coord map) of 
                     '|' -> North
                     '7' -> West
                     'F' -> East
                     _ -> trace (show $ coordToChar coord map) $ error "Dum dum"
    | dir == South = case (coordToChar coord map) of 
                     '|' -> South
                     'J' -> West
                     'L' -> East
                     _ -> error "Dum dum"
    | dir == East = case (coordToChar coord map) of 
                     '-' -> East
                     'J' -> North
                     '7' -> South
                     _ -> error "Dum dum"
    | dir == West = case (coordToChar coord map) of 
                     '-' -> West
                     'L' -> North
                     'F' -> South
                     _ -> error "Dum dum"

-- upMap :: [[Char]] -> Coord -> Coord -> [[Char]]
-- upMap [] _ _ = []
-- upMap ((x:xs):ys) curCoord changeCoord
--     | xs == [] = [value] : [upMap ys (curCoord {yCoord = yCoord curCoord + 1, xCoord = 0})] changeCoord
--     | otherwise = [[value]] ++ upMap ((xs):ys) (curCoord {xCoord = xCoord curCoord + 1}) changeCoord
--     where
--         value = if curCoord == changeCoord then 'Y' else x

replace :: (a -> a) -> Int -> [a] -> [a]
replace f 0 (x:xs) = (f x):xs
replace f i (x:xs) = x : replace f (i-1) xs
replace f i [] = []

replace2D :: (a -> a) -> (Int, Int) -> [[a]] -> [[a]]
replace2D f (x,y) = replace (replace f y) x

upMap :: [[Char]] -> Coord -> [[Char]]
upMap map coord = replace2D (const 'Y') (xCoord coord, yCoord coord) map

dijkstraIter :: [[Char]] -> Dijkstra -> (Dijkstra, [[Char]])
dijkstraIter map dijkstra
    | coordToChar (coord dijkstra) map == 'S' && passed dijkstra /= [] = (dijkstra, map)
    | otherwise = dijkstraIter map $ Dijkstra newStep newDir newCoord newPassed
    where
        newDir = nextDir (dir dijkstra) newCoord map
        newStep = step dijkstra + 1
        newCoord = dirToCoord dijkstra map
        newPassed = passed dijkstra ++ [coordToChar (coord dijkstra) map]

findS :: [[Char]] -> Coord -> Coord
findS ((x:xs):ys) coord
    | x == 'S' = coord
    | xs == [] = findS ys (coord {yCoord = yCoord coord + 1, xCoord = 0})
    | otherwise = findS ((xs):ys) (coord {xCoord = xCoord coord + 1})