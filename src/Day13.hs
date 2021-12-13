module Day13 where

import Utils
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Map as Map

data Direction = X | Y deriving (Show, Eq)

type Position = (Int, Int)
type FoldInstruction = (Direction, Int)
type PositionMap = Map.Map Position Int

page1 = [
    "6,10",
    "0,14",
    "9,10",
    "0,3",
    "10,4",
    "4,11",
    "6,0",
    "6,12",
    "4,1",
    "0,13",
    "10,12",
    "3,4",
    "3,0",
    "8,4",
    "1,10",
    "2,14",
    "8,10",
    "9,0",
    "",
    "fold along y=7",
    "fold along x=5"
    ]

parseFoldInstruction :: String -> FoldInstruction
parseFoldInstruction s = case c of
    "x" -> (X, read n)
    "y" -> (Y, read n)
    where (_:_:d:[]) = splitBy ' ' s
          (c:n:[]) = splitBy '=' d

tuple (a:b:[]) = (a,b)

parseInput :: [String] -> ([Position], [FoldInstruction])
parseInput rows = (positions, foldInstructions)
    where posInputs  = takeWhile (/= "") rows
          foldInputs = drop 1 $ dropWhile (/= "") rows
          positions :: [Position]
          parts :: [[Int]]
          parts = map (\s -> map read $ splitBy ',' s :: [Int]) posInputs
          positions  = map tuple parts
          foldInstructions = map parseFoldInstruction foldInputs

buildMap :: [Position] -> PositionMap
buildMap pos = Map.fromList input
    where input = zip pos (repeat 1)

foldHoriz :: Int -> (Position, Int) -> (Position, Int)
foldHoriz n ((x,y), v) = if y > n then ((x, newY y), v) else ((x,y), v)
    where newY oldY = n - (oldY - n)

foldVert :: Int -> (Position, Int) -> (Position, Int)
foldVert n ((x,y), v) = if x > n then ((newX x, y), v) else ((x,y), v)
    where newX oldX = n - (oldX - n)

foldPaper m (Y, pos) = Map.fromList $ filter (\((_,y), _) -> y /= pos) $ map (foldHoriz pos) $ Map.toList m
foldPaper m (X, pos) = Map.fromList $ filter (\((x,_), _) -> x /= pos) $ map (foldVert pos) $ Map.toList m

solveP1 :: [String] -> Int
solveP1 rows = Map.size m
    where (positions, foldInstructions) = parseInput rows
          paper = buildMap positions
          m = foldPaper paper (foldInstructions !! 0)

solveP2 rows = (++) "\n" $ unlines $ printMap m
    where (positions, foldInstructions) = parseInput rows
          paper = buildMap positions
          m = foldl foldPaper paper (foldInstructions)

printIt :: PositionMap -> Int -> Int -> Int -> Int -> String -> [String] -> [String]
printIt m x y maxX maxY row acc = case (x <= maxX, y <= maxY) of
    (True, _)     -> printIt m (x+1) y maxX maxY (row ++ [c]) acc
    (False, True) -> printIt m 0 (y+1) maxX maxY "" (acc ++ [row])
    (False, False) -> acc
    where c = maybe ' ' (\_ -> '•') $ Map.lookup (x,y) m

printMap :: PositionMap -> [String]
printMap m = printIt m 0 0 maxX maxY "" []
    where l = Map.toList m
          ((_, maxY), _) = maximumBy (comparing (snd . fst)) l
          ((maxX, _), _) = maximumBy (comparing (fst . fst)) l

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day17.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ p2)


