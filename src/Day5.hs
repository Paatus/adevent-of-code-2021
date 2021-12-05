module Day5 where

import Utils
import Debug.Trace
import qualified Data.Map as Map

data Coord = Coord { x :: Int, y :: Int } deriving (Show, Eq, Ord)

testInput = [
    "0,9 -> 5,9",
    "8,0 -> 0,8",
    "9,4 -> 3,4",
    "2,2 -> 2,1",
    "7,0 -> 7,4",
    "6,4 -> 2,0",
    "0,9 -> 2,9",
    "3,4 -> 1,4",
    "0,0 -> 8,8",
    "5,5 -> 8,2"
    ]

-- x0,y0 -> x1,y1
parseRow :: String -> (Coord, Coord)
parseRow s = (Coord x0 y0, Coord x1 y1)
    where w = words s
          first = w !! 0
          second = w !! 2
          (x0:y0:[]) = map read $ splitBy ',' first
          (x1:y1:[]) = map read $ splitBy ',' second

generateCoords :: [Coord] -> (Coord, Coord) -> [Coord]
generateCoords acc (a, b)
    | a == b = b : acc
    | otherwise =
    if isHorizontal (a, b) then
        case x0 > x1 of
            True -> generateCoords (a:acc) (Coord (x0 - 1) y0, b)
            False -> generateCoords (a:acc) (Coord (x0 + 1) y0, b)
    else if isVertical (a, b) then
        case y0 > y1 of
            True -> generateCoords (a:acc) (Coord x0 (y0 - 1), b)
            False -> generateCoords (a:acc) (Coord x0 (y0 + 1), b)
    else
        case (x0 > x1, y0 > y1) of
            (True, True) -> generateCoords (a:acc) (Coord (x0 - 1) (y0 - 1), b)   -- up left
            (False, True) -> generateCoords (a:acc) (Coord (x0 + 1) (y0 - 1), b)  -- up right
            (True, False) -> generateCoords (a:acc) (Coord (x0 - 1) (y0 + 1), b)  -- down left
            (False, False) -> generateCoords (a:acc) (Coord (x0 + 1) (y0 + 1), b) -- down right
    where (x0, y0) = (x a, y a)
          (x1, y1) = (x b, y b)

isHorizontal :: (Coord, Coord) -> Bool
isHorizontal (a, b) = y a == y b

isVertical :: (Coord, Coord) -> Bool
isVertical (a, b) = x a == x b

countElems :: (Ord a) => [a] -> Map.Map a Int
countElems = Map.fromListWith (+) . flip zip (repeat 1)

solver :: ((Coord, Coord) -> Bool) -> [String] -> Int
solver filterFun rows = Map.size $ Map.filter (>= 2) countMap
    where coordPairs = filter filterFun $ map parseRow rows
          countMap = countElems $ coordPairs >>= generateCoords []

solveP1 :: [String] -> Int
solveP1 rows = solver (\c -> isVertical c || isHorizontal c) rows

solveP2 :: [String] -> Int
solveP2 rows = solver (\_ -> True) rows

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day5.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)

