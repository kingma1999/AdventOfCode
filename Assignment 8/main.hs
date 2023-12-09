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

data Instr = Left | Right deriving (Eq, Show, Enum, Ord)

data Node = Node {
    self :: String,
    dir :: [String]
} deriving (Show)

instance Eq Node where
    (==) left right = self left == self right
    (/=) left right = self left == self right

instance Ord Node where
    compare left right = compare (self left) (self right)

main = do  
    handle <- openFile "input.txt" ReadMode
    contents <- hGetContents handle
    let linesOfFiles = lines contents
        instrList = map parseInstruction $ head linesOfFiles
        nodes = map parseNode $ map words $ map (filter (\x -> and [x/='=', x/='(', x/=')', x/=','])) $ tail $ tail linesOfFiles
        -- (steps, takenNodes) = findZZZ nodes instrList 0 [fromJust $ find (==(Node "AAA" [])) nodes]
        nodesEndingInA = nodeEndInA nodes
        stepsZ = map (\x -> findZ nodes instrList 0 x) nodesEndingInA
    print instrList
    -- print nodes
    -- print takenNodes
    -- print steps
    print nodesEndingInA
    print stepsZ
    print $ foldl1 lcm stepsZ
    hClose handle

makePair :: Node -> (Node, [Node])
makePair x = (x, [x])

parseInstruction :: Char -> Instr
parseInstruction char
    | char == 'L' = Main.Left
    | char == 'R' = Main.Right
    | otherwise = trace [char] $ error "Dum dum"

parseNode :: [String] -> Node
parseNode string = Node (string!!0) [string!!1, string!!2]

accessInstr :: [Instr] -> Int -> Instr
accessInstr instrs num = instrs!!(mod num (length instrs))

findZZZ :: [Node] -> [Instr] -> Int -> [Node] -> (Int, [Node])
findZZZ nodes instrs steps nodesTaken 
    | last nodesTaken == Node "ZZZ" [] = (steps, nodesTaken)
    | otherwise = findZZZ nodes instrs (steps + 1) newNodesTaken
    where
        newNodesTaken = nodesTaken ++ [newNode]
        findNode = Node ((dir $last nodesTaken)!!(fromEnum $ accessInstr instrs steps)) []
        newNode = fromJust $ find (==findNode) nodes

allNodesEndIn :: [Node] -> Char -> Bool
allNodesEndIn [] _ = True
allNodesEndIn (node:nodes) search 
    | last (self node) /= search = False
    | otherwise = allNodesEndIn nodes search

nodeEndInA :: [Node] -> [[Node]]
nodeEndInA [] = []
nodeEndInA (node:nodes) 
    | allNodesEndIn [node] 'A' = [node] : nodeEndInA nodes
    | otherwise = nodeEndInA nodes

findZ :: [Node] -> [Instr] -> Int -> [Node] -> Int
findZ nodes instrs steps nodesTaken 
    | allNodesEndIn [(last nodesTaken)] 'Z' = steps
    | otherwise = findZ nodes instrs (steps + 1) newNodesTaken
    where
        newNodesTaken = nodesTaken ++ [newNode]
        findNode = Node ((dir $last nodesTaken)!!(fromEnum $ accessInstr instrs steps)) []
        newNode = fromJust $ find (==findNode) nodes