module Day12 where

import Utils
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Char as C

testInput1 = [
    "start-A",
    "start-b",
    "A-c",
    "A-b",
    "b-d",
    "A-end",
    "b-end"
    ]

type PathMap = Map.Map String [String]

generatePathMap :: [String] -> PathMap
generatePathMap rows = generatePathMap' rows Map.empty

insertOrAppend :: String -> Maybe [String] -> Maybe [String]
insertOrAppend new mArr = case mArr of
    Nothing  -> Just [new]
    Just arr -> Just (new : arr)

walk :: PathMap -> [String] -> [String] -> [String] -> String -> [[String]]
walk m [] _ _ _ = []
walk m (node:nodes) path visited p2
    | node == "end"                = [path ++ ["end"]] ++ walk m nodes path visited p2
    | isUpper node                 = walk m (m Map.! node) (path++[node]) visited p2 ++ walk m nodes path visited p2
    | notElem node visited         = walk m (m Map.! node) (path++[node]) (node:visited) p2 ++ walk m nodes path visited p2
    | p2 == "" && node /= "start"  = walk m (m Map.! node) (path++[node]) visited node ++ walk m nodes path visited p2
    | otherwise                    = walk m nodes path visited p2


generatePathMap' :: [String] -> PathMap -> PathMap
generatePathMap' [] m = m
generatePathMap' (row:rows) m = generatePathMap' rows $ Map.alter (insertOrAppend b) a mm
    where (a:b:[]) = splitBy '-' row
          mm = Map.alter (insertOrAppend a) b m


isUpper :: String -> Bool
isUpper s = all (not . C.isLower) s

solveP1 :: [String] -> Int
solveP1 rows = length $ walk m ["start"] [] [] "."
    where m = generatePathMap rows

solveP2 :: [String] -> Int
solveP2 rows = length $ walk m ["start"] [] [] ""
    where m = generatePathMap rows

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day12.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
