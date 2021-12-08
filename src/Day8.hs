module Day8 where

import Data.Char
import Data.Maybe
import Debug.Trace
import Utils
import qualified Data.Map as Map
import Data.List

-- SEGMENTS
--  000
-- 1   2
--  333
-- 4   5
--  666


testInput = [
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe",
    "edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc",
    "fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg",
    "fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb",
    "aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea",
    "fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb",
    "dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe",
    "bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef",
    "egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb",
    "gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce"
    ]

getOutputs :: String -> [String]
getOutputs s = outputs
    where (i:o:[]) = splitBy '|' s
          outputs = words o

getInputs :: String -> [String]
getInputs s = inputs
    where (i:o:[]) = splitBy '|' s
          inputs = words i

solveP1 :: [String] -> Int
solveP1 s = length $ filter (isEasy) $ s >>= getOutputs

isEasy :: String -> Bool
isEasy s = case length s of
    2 -> True
    3 -> True
    4 -> True
    7 -> True
    _ -> False

createNumMap :: Map.Map String Int -> [String] -> Map.Map String Int
createNumMap numMap [] = numMap
createNumMap numMap (n:ns) =
    case (length n, fourOverLap,  oneOverLap) of
        (2, _, _) -> createNumMap (Map.insert sortedN 1 numMap) ns
        (3, _, _) -> createNumMap (Map.insert sortedN 7 numMap) ns
        (4, _, _) -> createNumMap (Map.insert sortedN 4 numMap) ns
        (7, _, _) -> createNumMap (Map.insert sortedN 8 numMap) ns
        (5, 2, _) -> createNumMap (Map.insert sortedN 2 numMap) ns
        (5, 3, 1) -> createNumMap (Map.insert sortedN 5 numMap) ns
        (5, 3, 2) -> createNumMap (Map.insert sortedN 3 numMap) ns
        (6, 4, _) -> createNumMap (Map.insert sortedN 9 numMap) ns
        (6, 3, 1) -> createNumMap (Map.insert sortedN 6 numMap) ns
        (6, 3, 2) -> createNumMap (Map.insert sortedN 0 numMap) ns
        _         -> createNumMap numMap (ns ++ [n])
    where sortedN = sort n
          numMapList = Map.toList numMap
          one :: String
          one = maybe "" fst $ find ((== 1) . snd) numMapList
          four = maybe "" fst $ find ((== 4) . snd) numMapList
          fourOverLap = overlap n four
          oneOverLap = overlap n one

overlap :: String -> String -> Int
overlap s1 s2 = Map.size $ Map.filter (>= 2) $ createCountMap (s1 ++ s2)

getRowValue :: ([String], [String]) -> Int
getRowValue (inputs, outputs) = read $ map (intToDigit . getNum numMap) outputs
    where numMap = createNumMap Map.empty inputs
          getNum :: Map.Map String Int -> String -> Int
          getNum m s = Map.findWithDefault (-1) (sort s) m

solveP2 :: [String] -> Int
solveP2 rows = sum $ map (\r -> getRowValue (getInputs r, getOutputs r)) rows

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day8.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
