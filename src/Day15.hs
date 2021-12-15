module Day15 where

import qualified Data.Map as Map
import Data.Char (digitToInt)
import qualified Data.Bifunctor as B
import Data.Maybe (fromJust, isJust, catMaybes, mapMaybe, fromMaybe)
import Debug.Trace (trace)
import Data.List (sortBy)
import Data.Ord (comparing)

testRiskMap = [
    "1163751742",
    "1381373672",
    "2136511328",
    "3694931569",
    "7463417111",
    "1319128137",
    "1359912421",
    "3125421639",
    "1293138521",
    "2311944581"
    ]


type Coord = (Int, Int) -- (X, Y)

generateRiskMap :: [String] -> Map.Map Coord Int
generateRiskMap rows = Map.fromList $ zip [0..] rows >>= createRow
    where createRow :: (Int, String) -> [(Coord, Int)]
          createRow (n, str) = let positions = zip (repeat n) [0..length str-1]
                                   nums :: [Int]
                                   nums = map digitToInt str
                               in zip positions nums

stepVal :: Int -> Int
stepVal x = if (x + 1) > 9 then (x+1)-9 else x+1

replicateX :: Int -> Map.Map Coord Int -> Map.Map Coord Int
replicateX times m = Map.union m newM
    where newM = Map.fromList $ Map.toList m >>= generateOffsets
          generateOffsets :: (Coord, Int) -> [(Coord, Int)]
          generateOffsets ((x,y), val) = map (\t -> ((x + (height*t), y), iterate stepVal val !! t)) [1..times-1]
          height = Map.size $ Map.filterWithKey (\(_,y) _ -> y == 0) m

replicateY :: Int -> Map.Map Coord Int -> Map.Map Coord Int
replicateY times m = Map.union m newM
    where newM = Map.fromList $ Map.toList m >>= generateOffsets
          generateOffsets :: (Coord, Int) -> [(Coord, Int)]
          generateOffsets ((x,y), val) = map (\t -> ((x, y+(width*t)), iterate stepVal val !! t)) [1..times-1]
          width = Map.size $ Map.filterWithKey (\(x,y) _ -> x == 0) m

generateBigRiskMap :: [String] -> Map.Map Coord Int
generateBigRiskMap rows = replicateY 5 $ replicateX 5 $ generateRiskMap rows

getStart :: Map.Map Coord Int -> Coord
getStart = fst . Map.findMin

getEnd :: Map.Map Coord Int -> Coord
getEnd = fst . Map.findMax

getNeighbours :: Map.Map Coord Int -> Coord -> [(Coord, Int)]
getNeighbours m (x,y) = map (B.second fromJust) $ filter (isJust . snd) $ map (\c -> (c, Map.lookup c m)) [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

heuristicFun :: Map.Map Coord Int -> Coord -> Int
heuristicFun m c = fromMaybe 999 $ Map.lookup c m

aStar :: Coord -> (Coord -> Bool) -> (Coord -> [(Coord, Int)]) -> (Coord -> Int) -> Maybe (Int, [Coord])
aStar start isEnd getNeighbours heuristic = aStar' [(heuristic start, start)] Map.empty (Map.singleton start 0) Map.empty
    where
        aStar' :: [(Int, Coord)] -> Map.Map Coord Int -> Map.Map Coord Int -> Map.Map Coord Coord -> Maybe (Int, [Coord])
        aStar' open closed gMap nodes
            | null open = Nothing
            | isEnd node = Just (gcost, findPath nodes node)
            | Map.member node closed = aStar' withoutNode closed gMap nodes
            | otherwise = aStar' newOpen newClosed newGMap newNodes
            where
                ((gcost, node):withoutNode) = open
                newClosed = Map.delete node closed
                notMemOrLessThanG :: Map.Map Coord Int -> Coord -> Int -> Bool
                notMemOrLessThanG m s g = let val = Map.lookup s m
                                          in case val of
                                            Nothing -> True
                                            Just a -> g < a
                successors = filter (\(s, g, _) -> Map.notMember s newClosed && notMemOrLessThanG gMap s g) $ successorsAndCosts node gcost
                successorsAndCosts :: Coord -> Int -> [(Coord, Int, Int)]
                successorsAndCosts node gc = map (\(c, g) -> (c, gc + g, heuristic c)) $ getNeighbours node
                newOpen = sortBy (comparing fst) $ foldl (\l (n, g, h) -> (g+h, n) : l) withoutNode successors
                newGMap = foldl (\m (n, g, _) -> Map.insert n g m) gMap successors
                newNodes = foldl (\m (n, g, _) -> Map.insert n node m) nodes successors
                findPath tracks node          = if Map.member node tracks
                  then findPath tracks (fromJust . Map.lookup node $ tracks) ++ [node]
                  else [node]

solver :: Map.Map Coord Int -> Int
solver m = case aStar start (== end) (getNeighbours m) (heuristicFun m) of
    Nothing -> -1
    Just (n, nodes) -> flip (-) startRisk $ sum $ mapMaybe (`Map.lookup` m) nodes
    where start = getStart m
          end = getEnd m
          startRisk = fromJust $ Map.lookup start m

solveP1 :: [String] -> Int
solveP1 rows = solver m
    where m = generateRiskMap rows

solveP2 :: [String] -> Int
solveP2 rows = solver m
    where m = generateBigRiskMap rows

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day15.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
