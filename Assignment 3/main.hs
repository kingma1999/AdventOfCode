import System.IO  
import Control.Monad
import Data.Char
import Data.List.Split
import Debug.Trace

data Coord = Coord { xloc :: Int,
                        yloc :: Int} deriving (Show, Eq)

data Symb = Symb { char :: Char,
                    coordS :: Coord,
                    parts :: [Int]} deriving (Show, Eq)

data EnginePart = EnginePart { partNumber :: Int,
                                adjacentSymbols :: [Symb],
                                coord :: Coord} deriving (Show, Eq)

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        partsList = findPartsY linesOfFiles (Coord 0 0)
        som = sum (validPartNums partsList)
        symbPartsList = takePartNumsDown partsList
        symbsList = makeSymbsList symbPartsList
        symbsWithParts = symblifier symbsList
        somGear = sum (gearMultiply symbsWithParts)
    print symbsWithParts
    print som
    print somGear
    hClose handle

list2D :: [[Char]] -> Coord -> Char
list2D input loc = ((input!!(yloc loc))!!(xloc loc))

isSymb :: Char -> Bool
isSymb x
    | x < '.' && ord (x) >= 33 = True
    | x > '.' && x < '0' = True
    | x > '9' && x < 'A' = True
    | x > 'Z' && x < 'a' = True
    | x > 'z' && ord (x) <= 127 = True
    | otherwise = False

concatTwoIntToInt :: Int -> Int -> Int
concatTwoIntToInt n1 n2 = read ((show n1) ++ (show n2))

eODHelper :: [Char] -> Int -> Int
eODHelper [] y = y
eODHelper (x:xs) y
    | isDigit x = eODHelper xs (y + 1)
    | otherwise = y

endOfDigit :: [Char] -> Int
endOfDigit input = eODHelper input 0 

partNumHelper :: [Char] -> [Char]
partNumHelper [] = []
partNumHelper (x:xs)
    | isDigit x = x : partNumHelper xs
    | otherwise = []

partNum :: [Char] -> Int
partNum input = read (partNumHelper input)

fASX :: [Char] -> Coord -> [Symb]
fASX [] _ = []
fASX (x:xs) loc
    | isSymb x = Symb x loc [] : fASX xs (loc {xloc = xloc loc + 1})
    | otherwise = fASX xs (loc {xloc = xloc loc + 1})

findAllSymbols :: [[Char]] -> Coord -> [Symb]
findAllSymbols (y:ys) loc
    | ys == [] = fASX y loc
    | otherwise = fASX y loc ++ findAllSymbols ys (loc {yloc = yloc loc + 1})

slice :: Int -> Int -> [a] -> [a]
slice from to xs = take (to - from + 1) (drop from xs)

boundsCheck :: Int -> Int -> Int
boundsCheck attempt range
    | attempt < 0 = 0
    | attempt > range - 1 = range - 1
    | otherwise = attempt

cutX :: [[Char]] -> Coord -> Int -> [[Char]]
cutX [] _ _ = []
cutX (y:ys) loc len = slice (boundsCheck (xloc loc - 1) (length y)) (boundsCheck (xloc loc + len) (length y)) y : cutX ys loc len

findSymbols :: [[Char]] -> Coord -> Int -> [Symb]
findSymbols [] _ _ = []
findSymbols input loc len = findAllSymbols xycut (loc {yloc = (boundsCheck (yloc loc - 1) (length input)), xloc = (boundsCheck (xloc loc - 1) (length (input!!0)))})
    where 
        ycut = slice (boundsCheck (yloc loc - 1) (length input)) (boundsCheck (yloc loc + 1) (length input)) input
        xycut = cutX ycut loc len

findPartsX :: [[Char]] -> Coord -> [EnginePart]
findPartsX input loc
    | xloc loc == (length (input!!(yloc loc))) = []
    | isDigit ((input!!(yloc loc))!!(xloc loc)) = EnginePart (partNum rest) (findSymbols input loc (endOfDigit rest)) loc : findPartsX input (loc { xloc = xloc loc + (endOfDigit rest)})
    | otherwise = findPartsX input (loc { xloc = xloc loc + 1})
        where rest = (drop (xloc loc) (input!!(yloc loc)))

findPartsY :: [[Char]] -> Coord -> [EnginePart]
findPartsY input loc
    | yloc loc == (length input) = []
    | otherwise = findPartsX input loc ++ findPartsY input (loc { yloc = yloc loc + 1})

validPartNums :: [EnginePart] -> [Int]
validPartNums [] = []
validPartNums (x:xs)
    | adjacentSymbols x == [] = validPartNums xs
    | otherwise = partNumber x : validPartNums xs

tPNDHelper :: [Symb] -> Int -> [Symb]
tPNDHelper [] _ = []
tPNDHelper (x:xs) int = x {parts = [int]} : tPNDHelper xs int

takePartNumsDown :: [EnginePart] -> [EnginePart]
takePartNumsDown [] = []
takePartNumsDown (x:xs) = x {adjacentSymbols = tPNDHelper (adjacentSymbols x) (partNumber x)} : takePartNumsDown xs

makeSymbsList :: [EnginePart] -> [Symb]
makeSymbsList [] = []
makeSymbsList (ePart:eParts) = (adjacentSymbols ePart) ++ (makeSymbsList eParts)

symblifierHelper :: [Symb] -> Symb -> Symb
symblifierHelper [] ref = ref
symblifierHelper (symb:symbs) ref 
    | coordS symb == coordS ref = symblifierHelper symbs (ref {parts = (parts ref) ++ (parts symb)})
    | otherwise = symblifierHelper symbs ref

symblifier :: [Symb] -> [Symb]
symblifier [] = []
symblifier (symb:symbs) = symblifierHelper symbs symb : symblifier symbs

gearMultiply :: [Symb] -> [Int]
gearMultiply [] = []
gearMultiply (symb:symbs) 
    | char symb == '*' && (length (parts symb)) == 2 = ((parts symb)!!0) * ((parts symb)!!1) : gearMultiply symbs
    | otherwise = gearMultiply symbs