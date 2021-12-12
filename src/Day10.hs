module Day10 where

import Debug.Trace
import Utils
import Data.List
import qualified Data.Map as Map
import Data.Maybe

-- [({(<(())[]>[[{[]{<()<>>

-- {([(<{}[<>[]}>{[]{[(<()>
-- { ( [(<{}[<>[]}>{[]{[(<()>

testInput = [
    "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
    ]

isOpenChar :: Char -> Bool
isOpenChar c = case c of
    '(' -> True
    '<' -> True
    '{' -> True
    '[' -> True
    _   -> False


isClosingChar :: Char -> Bool
isClosingChar c = case c of
    ')' -> True
    '>' -> True
    '}' -> True
    ']' -> True
    _   -> False


getClosingChar :: Char -> Char
getClosingChar c = case c of
    '(' -> ')'
    '<' -> '>'
    '{' -> '}'
    '[' -> ']'

equalsClosingChar :: Char -> Char -> Bool
equalsClosingChar c c2 = c2 == (getClosingChar c)

getScore1 :: Char -> Int
getScore1 c = case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137

getScore2 :: Char -> Int
getScore2 c = case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4

getCompletion :: String -> String
getCompletion s = map getClosingChar (reverse s)

scoreCompletion :: String -> Int
scoreCompletion s = foldl (\acc c -> (+ (getScore2 c)) $ acc * 5) 0 s

getPartialSequence :: String -> String -> String
getPartialSequence acc [] = acc
getPartialSequence acc (s:[]) = acc ++ [s]
getPartialSequence acc (s:s2:ss) =
    case (isOpenChar s, isClosingChar s2) of
        (True, True)  -> if equalsClosingChar s s2
                            then getPartialSequence [] (acc ++ ss)
                            else [s2]
        (True, False) -> getPartialSequence (acc ++ [s]) ([s2] ++ ss)
        (False, _)    -> getPartialSequence [] (acc ++ [s,s2] ++ ss)


findFirstInvalidChar :: String -> String -> Maybe Char
findFirstInvalidChar acc [] = Nothing
findFirstInvalidChar acc (s:[]) = Nothing
findFirstInvalidChar acc (s:s2:ss) =
    case (isOpenChar s, isClosingChar s2) of
        (True, True)  -> if equalsClosingChar s s2
                            then findFirstInvalidChar [] (acc ++ ss)
                            else Just s2
        (True, False) -> findFirstInvalidChar (acc ++ [s]) ([s2] ++ ss)
        (False, _)    -> findFirstInvalidChar [] (acc ++ [s,s2] ++ ss)


solveP1 :: [String] -> Int
solveP1 rows = sum $ map (maybe 0 getScore1 . findFirstInvalidChar []) rows

solveP2 rows = sortedScores !! midIndex
    where scores = map (scoreCompletion . getCompletion . getPartialSequence []) $ filter (isNothing . findFirstInvalidChar []) rows
          sortedScores = sort scores
          midIndex = length sortedScores `div` 2


solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day10.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
