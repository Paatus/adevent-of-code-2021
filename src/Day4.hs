module Day4 where

import Utils

data Tile = Tile { marked:: Bool, value:: Int } deriving (Show)
type Board = [Tile]

testBoardStr = [
    "22 13 17 11  0",
    " 8  2 23  4 24",
    "21  9 14 16  7",
    " 6 10  3 18  5",
    " 1 12 20 15 19"]

mkTile :: Bool -> Int -> Tile
mkTile marked n = Tile marked n

parseBoards :: [String] -> Board -> [Board] -> [Board]
parseBoards (_:[]) currBoard acc = (acc ++ [currBoard])
parseBoards ("":xs) currBoard acc = parseBoards xs [] (acc ++ [currBoard])
parseBoards (x:xs) currBoard acc = parseBoards xs (currBoard ++ (map (mkTile False . read) $ words x)) acc

hasTile :: Int -> Board -> Bool
hasTile num board = (> 0) $ length $ filter (\t -> (== num) $ value t) board

fillBoard :: Int -> Board -> Board
fillBoard n board =
    map (\tn -> if (value tn) == n then mkTile True n else tn) board

winCheck :: [[Tile]] -> Bool
winCheck [] = False
winCheck (p:ps) =
    if (length markedTiles) == (length p) then
        True
    else
        winCheck ps
    where
        markedTiles = filter marked p

boardHasWon :: Board -> Bool
boardHasWon board =
    let horizontals = map (map (\n -> board !! n)) $ [[0..4], [5..9], [10..14], [15..19], [20..24]]
        verticals = map (map (\n -> board !! n)) $ [take 5 [0,5..], take 5 [1,6..], take 5 [2,7..], take 5 [3,8..], take 5 [4,9..]]
        horizontalWinner = winCheck horizontals
        verticalWinner = winCheck verticals
    in
        verticalWinner || horizontalWinner

getWinner :: [Board] -> [Board]
getWinner boards =
    filter boardHasWon boards

solveP1 :: [Int] -> [Board] -> Int
solveP1 (n:ns) boards =
    let evolvedBoards = map (fillBoard n) boards
        winBoards = getWinner evolvedBoards
        hasWinner = (> 0) $ length winBoards
        winnerBoard :: Board
        winnerBoard = winBoards !! 0
    in
    if hasWinner then
        calcBoardScore winnerBoard n
    else
        solveP1 ns evolvedBoards
solveP1 [] boards = -1

solveP2 :: [Int] -> [Board] -> Int
solveP2 ns (b:[]) = solveP1 ns [b]
solveP2 (n:ns) boards =
    let evolvedBoards = map (fillBoard n) boards
        badBoards = filter (not . boardHasWon) evolvedBoards
    in
        solveP2 ns badBoards

calcBoardScore :: Board -> Int -> Int
calcBoardScore board n = n * sumUnmarked
    where sumUnmarked = sum $ map value $ filter (not . marked) board

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day4.txt"
    let rows = lines fileContents
        numbers :: [Int]
        numbers = map read $ splitBy ',' (rows !! 0)
        boards  = parseBoards (drop 2 rows) [] []
        p1 = solveP1 numbers boards
        p2 = solveP2 numbers boards
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
