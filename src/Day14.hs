{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# LANGUAGE TupleSections #-}
module Day14 where

import Utils
import Data.List
import Data.Ord
import qualified Data.Map as Map
import Debug.Trace

testInput = [
    "NNCB",
    "",
    "CH -> B",
    "HH -> N",
    "CB -> H",
    "NH -> C",
    "HB -> C",
    "HC -> B",
    "HN -> C",
    "NN -> C",
    "BH -> H",
    "NC -> B",
    "NB -> B",
    "BN -> B",
    "BB -> N",
    "BC -> B",
    "CC -> N",
    "CN -> C"
    ]

(template, m) = parseInput testInput
im = createCountMap template

type ReplacementMap = Map.Map String Char

parseInstruction :: String -> (String, Char)
parseInstruction s = (seq, c)
    where [seq, _, cs] = splitBy ' ' s
          c = head cs

parseInput :: [String] -> (String, ReplacementMap)
parseInput rows = (template, Map.fromList instructions)
    where template = head rows
          instructionRows = drop 1 $ dropWhile (/= "") rows
          instructions = map parseInstruction instructionRows

buildPolymer :: String -> ReplacementMap -> String -> String
buildPolymer [a] m acc = acc ++ [a]
buildPolymer (a:b:tail) m acc = buildPolymer (b:tail) m (acc ++ newSeq)
    where insertChar = Map.lookup [a,b] m
          newSeq = case insertChar of
                    Nothing -> [a]
                    Just c -> [a,c]

type CountMap = Map.Map String Int
type CombinationMap = Map.Map String [String]

convertToCombinationMap :: ReplacementMap -> CombinationMap
convertToCombinationMap rm = Map.fromList $ map (\([a,b], v) -> ([a,b], [[a,v],[v,b]])) $ Map.toList rm

toCombinationCountMap :: String -> CountMap
toCombinationCountMap t = Map.fromListWith (+) $ zipWith (\ a b -> ([a, b], 1)) (' ' : t) t

step :: CombinationMap -> CountMap -> CountMap
step cm cm2 =
  let resolve (p, c) = case cm Map.!? p of
        Just ps -> map (, c) ps
        Nothing -> [(p, c)]
   in Map.fromListWith (+) $ concatMap resolve $ Map.toList cm2


solveStep :: Int -> [String] -> Int
solveStep steps rows = common - rare
    where (template, replacementMap) = parseInput rows
          combinationMap = convertToCombinationMap replacementMap
          combinationCountMap = toCombinationCountMap template
          tenSteps = iterate (step combinationMap) combinationCountMap !! steps
          counts = Map.fromListWith (+) $ map (\(s, n) -> (s !! 1, n)) $ Map.toList tenSteps
          (_, common) = maximumBy (comparing snd) $ Map.toList counts
          (_, rare) = minimumBy (comparing snd) $ Map.toList counts

solveP1 :: [String] -> Int
solveP1 = solveStep 10

solveP2 = solveStep 40


solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day14.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
