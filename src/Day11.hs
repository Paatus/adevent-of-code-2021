module Day11 where

import Debug.Trace
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

testInput = [
    "5483143223",
    "2745854711",
    "5264556173",
    "6141336146",
    "6357385478",
    "4167524645",
    "2176841721",
    "6882881134",
    "4846848554",
    "5283751526"
    ]

step1 = [
    "6594254334",
    "3856965822",
    "6375667284",
    "7252447257",
    "7468496589",
    "5278635756",
    "3287952832",
    "7993992245",
    "5957959665",
    "6394862637"
    ]

step2 = [
    "8807476555",
    "5089087054",
    "8597889608",
    "8485769600",
    "8700908800",
    "6600088989",
    "6800005943",
    "0000007456",
    "9000000876",
    "8700006848"
    ]

type Position = (Int, Int) -- (x, y)

data Octopus = Dumbo Int Bool Position deriving (Show, Eq)

parseRow :: Int -> [Octopus] -> String -> [Octopus]
parseRow _ acc [] = acc
parseRow y acc (o:os) = parseRow y (acc ++ [dumbo]) os
    where x = 10 - (length os + 1)
          power = digitToInt o
          dumbo = Dumbo power False (x,y)

parseOctopuses rows = parseOctopuses' rows Map.empty
    where
        parseOctopuses' :: [String] -> Map.Map Position Octopus -> Map.Map Position Octopus
        parseOctopuses' [] m = m
        parseOctopuses' ([]:os) m = parseOctopuses' os m
        parseOctopuses' (s:os) m = parseOctopuses' (tail s:os) newMap
            where x = 10 - (length os + 1)
                  y = 10 - (length s)
                  o = head s
                  power = digitToInt o
                  dumbo = Dumbo power False (x,y)
                  newMap = Map.insert (x,y) dumbo m

getNeighbours :: Position -> [Position]
getNeighbours (x,y) = filter (\(x,y) -> x >= 0 && y >= 0 && x <= 9 && y <= 9) [
    (x-1, y-1), (x, y-1), (x+1, y-1),
    (x-1, y),             (x+1, y),
    (x-1, y+1), (x, y+1), (x+1, y+1)
    ]

-- showMap :: Map.Map Position Octopus
showMap m = putStrLn $ Map.toList m >>= (\((x,y), (Dumbo p _ _)) -> if y == 0 then "\n" ++ show p else show p)

resetOctopus :: Octopus -> Octopus
resetOctopus = resetPower . resetFlash

resetPower :: Octopus -> Octopus
resetPower (Dumbo power flashed pos) = if power > 9 then
        Dumbo 0 flashed pos
    else
        Dumbo power flashed pos

resetFlash :: Octopus -> Octopus
resetFlash (Dumbo power _ pos) = Dumbo power False pos

increasePower :: Octopus -> Octopus
increasePower (Dumbo pow flashed pos) = Dumbo (pow + 1) flashed pos

setFlashed' (Dumbo pwr flashed pos) = if pwr > 9 then
        Dumbo pwr True pos
    else
        Dumbo pwr flashed pos

flashOctopuses :: [Position] -> Map.Map Position Octopus -> Map.Map Position Octopus
flashOctopuses [] m = m
flashOctopuses (p:ps) m = flashOctopuses ps newM
    where mm = Map.adjust setFlashed' p m
          mmm = Map.adjust increasePower p mm
          o = fromJust $ Map.lookup p mmm
          newM = flashOctopus o mmm

shouldFlash :: Octopus -> Bool
shouldFlash (Dumbo power flashed _) = power > 9 && not flashed

flashOctopus :: Octopus -> Map.Map Position Octopus -> Map.Map Position Octopus
flashOctopus (Dumbo power flashed pos) m = if shouldFlash o then
        flashOctopuses flashNeighbours mm
    else
        mm
    where o = Dumbo power flashed pos
          mm = Map.adjust setFlashed' pos m
          flashNeighbours = getNeighbours pos

increase1 :: Map.Map Position Octopus -> Map.Map Position Octopus
increase1 m = Map.map increasePower m

flash :: Map.Map Position Octopus -> Map.Map Position Octopus
flash m = Map.foldr flashOctopus m m

reset :: Map.Map Position Octopus -> (Map.Map Position Octopus, Int)
reset m = (newM, zeroes)
    where newM = Map.map resetOctopus m
          zeroes = Map.size $ Map.filter (\(Dumbo p _ _) -> p == 0) newM

step :: (Map.Map Position Octopus, Int) -> (Map.Map Position Octopus, Int)
step (m, acc) = (newM, newAcc)
    where (newM, nAcc) = reset $ flash $ increase1 m
          newAcc = nAcc + acc

getPower :: Octopus -> Int
getPower (Dumbo p _ _) = p

allFlashing :: Map.Map Position Octopus -> Bool
allFlashing m = all ((==) 0 . getPower . snd) $ Map.toList m

solveP1 :: [String] -> Int
solveP1 rows = snd $ (iterate step (initialM, 0)) !! 100
    where initialM = parseOctopuses rows

solveP2 :: [String] -> Int
solveP2 rows = length $ takeWhile (not . allFlashing . fst) $ (iterate step (initialM, 0))
    where initialM = parseOctopuses rows

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day11.txt"
    let rows = lines fileContents
        p1 = solveP1 rows
        p2 = solveP2 rows
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
