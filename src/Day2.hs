module Day2 where

import Data.List
import Data.Char

data Direction = Forward | Up | Down deriving (Show, Eq)

debugInput = ["forward 5",
            "down 5",
            "forward 8",
            "up 3",
            "down 8",
            "forward 2"]

solveP1 :: [String] -> Int
solveP1 directions =
    solvePositions (map getDirection directions) (0, 0)
    where
        solvePositions ((direction, steps):xs) (y, x) =
            case direction of
                Up -> solvePositions xs (y - steps, x)
                Down -> solvePositions xs (y + steps, x)
                Forward -> solvePositions xs (y, x + steps)
        solvePositions [] (x,y) = x * y

solveP2 :: [String] -> Int
solveP2 directions =
    solvePositions (map getDirection directions) (0, 0, 0)
    where
        solvePositions ((direction, steps):xs) (y, x, aim) =
            case direction of
                Up -> solvePositions xs (y, x, aim - steps)
                Down -> solvePositions xs (y, x, aim + steps)
                Forward -> solvePositions xs (y + (steps * aim), x + steps, aim)
        solvePositions [] (x,y,_) = x * y

getDirection :: String -> (Direction, Int)
getDirection s =
    let steps = digitToInt $ head $ dropWhile (not . isDigit) s
    in
    case (isInfixOf "forward" s, isInfixOf "down" s) of
        (True, _) -> (Forward, steps)
        (_, True) -> (Down, steps)
        _         -> (Up, steps)

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day2.txt"
    let rows = lines fileContents
    putStrLn (
        "Part 1: " ++ show (solveP1 rows) ++ "\r\n"
        ++ "Part 2: " ++ show (solveP2 rows))
