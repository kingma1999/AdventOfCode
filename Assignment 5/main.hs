import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace

data Seed = Seed { 
    nums :: [Int]
} deriving (Show, Eq)

data Map = Map {
    source :: Int,
    destination :: Int,
    range :: Int
} deriving (Show, Eq)

data MapCol = MapCol {
    name :: String,
    maps :: [Map]
} deriving (Show, Eq)

data Range = Range {
    low :: Int,
    high :: Int
} deriving (Show, Eq)

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    print "starting"
    let linesOfFiles = lines contents
        -- seeds = map parseSeed $ tail $ words (linesOfFiles!!0)
        -- test = splitOn [""] (tail linesOfFiles)
        -- maps = parseMapCol $ filter (/=[]) $ filter (/=[""]) (splitOn [""] (tail linesOfFiles))
        -- betterSeeds = doMapsWrapper seeds maps
        -- closest = minimum $ map getLoc betterSeeds
        -- rangeSeeds = parseSeedRangesRev $ tail $ words (linesOfFiles!!0)
        -- betterSeedsRange = doMapsWrapper rangeSeeds maps
        -- closestRange = minimum $ map getLoc betterSeedsRange
        -- unmappedSeeds = (seedUnMapper rangeSeeds)
        ranges = rangeCollector $ tail $ words (linesOfFiles!!0)
        reverseMaps = parseMapColRev $ filter (/=[]) $ filter (/=[""]) (splitOn [""] (tail linesOfFiles))
        score = lowestFinderRev reverseMaps (Seed [0]) ranges
    -- print seeds
    -- print maps
    -- print betterSeeds
    -- print closest
    -- print ""
    -- print rangeSeeds
    print ranges
    print ("final: " ++ show (score))
    hClose handle

rangeCollector :: [String] -> [Range]
rangeCollector [] = []
rangeCollector (num1:num2:nums) = trace ("Oehh, betah") ([Range int1 (int1 + int2 - 1)] ++ rangeCollector nums)
    where
        int1 = read num1
        int2 = read num2

seedMapper :: [Int] -> [Seed]
seedMapper [] =[]
seedMapper (num:nums) = Seed [num] : seedMapper nums

parseSeedRangesRev :: [String] -> [Int]
parseSeedRangesRev [] = []
parseSeedRangesRev (num1:num2:nums) = trace ("Oehh, betah") ([int1..(int1 + int2 - 1)] ++ parseSeedRangesRev nums)
    where
        int1 = read num1
        int2 = read num2

parseSeedRanges :: [String] -> [Seed]
parseSeedRanges [] = []
parseSeedRanges (num1:num2:nums) = trace ("Oehh, lekkah") (seedMapper [int1..(int1 + int2 - 1)] ++ parseSeedRanges nums)
    where
        int1 = read num1
        int2 = read num2

parseSeed :: String -> Seed
parseSeed input = Seed [read input]

parseMap :: String -> Map
parseMap input = Map source dest range
    where
        [dest, source, range] = map read (words input)

parseMapCol :: [[String]] -> [MapCol]
parseMapCol [] = []
parseMapCol ((string:locs):strings) = MapCol name (map parseMap locs) : parseMapCol strings
    where
        name = trace ("lmfao") (head (words string))

isInRange :: Seed -> Map -> Int
isInRange seed ref
    | fin >= source ref && fin < (source ref) + (range ref) = (destination ref) + ((fin) - (source ref))
    | otherwise = fin
    where 
        fin = last $ nums seed

isInRangeRev2 :: Seed -> Map -> Int
isInRangeRev2 seed ref
    | fin >= source ref && fin < (source ref) + (range ref) = (destination ref) + ((fin) - (source ref))
    | otherwise = fin
    where 
        fin = last $ nums seed

doMapCol :: Seed -> [Map] -> Int
doMapCol seed [] = last $ nums seed
doMapCol seed (ref:refs)
    | (isInRangeRev2 seed ref) == last (nums seed) = doMapCol seed refs
    | otherwise = isInRangeRev2 seed ref

doMaps :: Seed -> [MapCol] -> [Int]
doMaps _ [] = []
doMaps seed (ref:refs) = intermediate : doMaps (seed {nums = (nums seed) ++ [intermediate]}) refs
    where
        intermediate = doMapCol seed (maps ref)

doMapsWrapper :: [Seed] -> [MapCol] -> [Seed]
doMapsWrapper [] _ = []
doMapsWrapper (seed:seeds) refs = Seed ([last (nums seed)] ++ (doMaps seed refs)) : doMapsWrapper seeds refs

getLoc :: Seed -> Int
getLoc seed = last $ nums seed

parseMapRev :: String -> Map
parseMapRev input = Map dest source range
    where
        [dest, source, range] = map read (words input)

parseMapColRev :: [[String]] -> [MapCol]
parseMapColRev [] = []
parseMapColRev ((string:locs):strings) = parseMapColRev strings ++ [MapCol name (map parseMapRev locs)]
    where
        name = head (words string)

-- isInRangeRev :: Seed -> Map -> Int
-- isInRangeRev seed ref
--     | fin >= source ref && fin < (source ref) + (range ref) = (destination ref) + ((fin) - (source ref))
--     | otherwise = fin
--     where 
--         fin = last $ nums seed

-- doMapColRev :: Seed -> [Map] -> Int
-- doMapColRev seed [] = last $ nums seed
-- doMapColRev seed (ref:refs)
--     | (isInRangeRev seed ref) == last (nums seed) = doMapColRev seed refs
--     | otherwise = isInRangeRev seed ref

-- doMapsRev :: Seed -> [MapCol] -> [Int]
-- doMapsRev _ [] = []
-- doMapsRev seed (ref:refs) = intermediate : doMapsRev (seed {nums = (nums seed) ++ [intermediate]}) refs
--     where
--         intermediate = doMapColRev seed (maps ref)

isInRangeRev :: Int -> [Range] -> Bool
isInRangeRev _ [] = False
isInRangeRev check (ref:refs)
    | check >= low ref && check <= high ref = True
    | otherwise = isInRangeRev check refs

lowestFinderRev :: [MapCol] -> Seed -> [Range] -> Int
lowestFinderRev maps seed ref
    | isInRangeRev (last performance) ref = head performance
    | otherwise = (lowestFinderRev maps (seed {nums = [printNum]}) ref)
        where 
            performance = (doMaps seed maps)
            printNum = ((nums seed)!!0) + 1