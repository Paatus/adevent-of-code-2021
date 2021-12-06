module Day6 where

import Utils
import Data.Maybe
import qualified Data.List as L
import qualified Data.Map as Map
import Debug.Trace

testInput = "3,4,3,1,2"

testMap = createCountMap $ parseInput testInput

parseInput :: String -> [Int]
parseInput input = map read $ splitBy ',' input

solveP1 :: [Int] -> Int
solveP1 nums = Map.foldr (+) 0 $ head $ drop 80 $ iterate doDay m
    where m = createCountMap nums

solveP2 :: [Int] -> Int
solveP2 nums = Map.foldr (+) 0 $ head $ drop 256 $ iterate doDay m
    where m = createCountMap nums

doDay :: Map.Map Int Int -> Map.Map Int Int
doDay m = Map.fromList newList
    where newList = mL >>= thing
          mL = Map.toList m
          thing (key, val) = case key of
                                0 -> [(8, zeroes), (6, zeroes + sevens)]
                                7 -> if zeroes == 0 then [(key - 1, val)] else []
                                _ -> [(key - 1, val)]
                            where
                                zeroes :: Int
                                zeroes = snd $ fromMaybe (0, 0) $ L.find (\(k,v) -> k == 0) mL
                                sevens = snd $ fromMaybe (7, 0) $ L.find (\(k,v) -> k == 7) mL


createCountMap :: (Ord a) => [a] -> Map.Map a Int
createCountMap = Map.fromListWith (+) . flip zip (repeat 1)

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day6.txt"
    let line = head $ lines fileContents
        p1 = solveP1 (parseInput line)
        p2 = solveP2 (parseInput line)
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
