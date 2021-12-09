module Day9 where

import qualified Data.Set as Set
import Data.Char
import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as List

type Position = (Int, Int) -- (x, y)
type DepthMap = Map.Map Position Int

testInput :: [String]
testInput = [
    "2199943210",
    "3987894921",
    "9856789892",
    "8767896789",
    "9899965678"
    ]

createDepthMap :: [String] -> Int -> DepthMap -> DepthMap
createDepthMap [] _ m = m
createDepthMap (r:rows) y m = createDepthMap rows (y+1) newMap
    where positions = zip [0..length r] (repeat y)
          depths = map digitToInt r
          newPositions = zip positions depths
          newMap = Map.union (Map.fromList newPositions) m

getNeighbourPositions (x,y) = [(x-1, y), (x+1, y), (x, y+1), (x, y - 1)]

getNeighbours :: DepthMap -> Position -> [Int]
getNeighbours m pos = catMaybes $ map (\pos -> Map.lookup pos m) (getNeighbourPositions pos)

isLowPoint :: DepthMap -> Position -> Int -> Bool
isLowPoint m pos val = (> val) $ minimum neighbours
    where neighbours = getNeighbours m pos

getLowPoints :: DepthMap -> [Position]
getLowPoints m = map fst $ Map.toList $ Map.filterWithKey (isLowPoint m) m

lookupWithKey :: DepthMap -> Position -> (Position, Maybe Int)
lookupWithKey m p = (p, Map.lookup p m)

getBasinNeighbours :: DepthMap -> [Position] -> Set.Set Position -> Int
getBasinNeighbours _ [] acc = (+1) $ Set.size acc
getBasinNeighbours m (p:ps) acc = getBasinNeighbours m (ps ++ newNeighbours) (Set.union acc $ Set.fromList newNeighbours)
    where currDepth = Map.lookup p m
          newNeighbours = filter (isHigher m currDepth) $ getNeighbourPositions p
          isHigher :: DepthMap -> Maybe Int -> Position -> Bool
          isHigher map mI pos =
            case mI of
                Nothing -> False
                Just n -> maybe False (\h -> h > n && h < 9) $ Map.lookup pos map

getBasinSize :: DepthMap -> Position -> Int
getBasinSize m pos = getBasinNeighbours m [pos] Set.empty

solveP2 :: [String] -> Int
solveP2 rows = foldl (*) 1 $ take 3 $ List.sortBy (\a b -> compare b a) $ map (getBasinSize dm) $ getLowPoints dm
    where dm = createDepthMap rows 0 Map.empty

solveP1 :: [String] -> Int
solveP1 rows = sum $ map ((+1). snd) $ Map.toList $ Map.filterWithKey (isLowPoint dm) dm
    where dm = createDepthMap rows 0 Map.empty

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day9.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
