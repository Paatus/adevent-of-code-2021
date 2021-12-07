module Day7 where

import Utils
import qualified Data.List as L
import Data.Ord

distDiff :: Int -> [Int] -> Int
distDiff target positions = sum $ map (abs . ((-) target)) positions

distDiff2 :: Int -> [Int] -> Int
distDiff2 target positions = sum $ map distFun positions
    where distFun n = d * (d+1) `div` 2
            where d = abs (n - target)

allDiffs :: [Int] -> (Int -> [Int] -> Int) -> [(Int, Int)]
allDiffs positions distFun = map (\t -> (t, distFun t positions)) [min..max]
    where min = minimum positions
          max = maximum positions

getBestPos :: [Int] -> (Int -> [Int] -> Int) -> (Int, Int)
getBestPos positions distFun = L.minimumBy (comparing snd) diffs
    where diffs = allDiffs positions distFun

solveP1 :: [Int] -> Int
solveP1 nums = cost
    where (_, cost) = getBestPos nums distDiff

solveP2 :: [Int] -> Int
solveP2 nums = cost
    where (_, cost) = getBestPos nums distDiff2

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day7.txt"
    let nums = map read $ splitBy ',' fileContents
        p1 = solveP1 nums
        p2 = solveP2 nums
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
