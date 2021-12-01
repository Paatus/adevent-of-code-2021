module Day1 where

solveP1 :: [Integer] -> Integer
solveP1 nums =
    solveAcc nums 0
    where
        solveAcc :: [Integer] -> Integer -> Integer
        solveAcc (h1:h2:t) acc = case h2 > h1 of
            True -> solveAcc (h2 : t) (acc + 1)
            False -> solveAcc (h2 : t) acc
        solveAcc [_] acc = acc


solveP2 :: [Integer] -> Integer
solveP2 nums =
    solveP1 $ map sum $ getSlices nums []


getSlices :: [Integer] -> [[Integer]] -> [[Integer]]
getSlices (h1:h2:h3:t) acc =
    let slice = h1 : h2 : [h3]
    in
    getSlices (h2:h3:t) (acc ++ [slice])
getSlices (h:t) acc = acc
getSlices [] acc = acc

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day1.txt"
    let nums = map read $ lines fileContents
        p1 = show $ solveP1 nums
        p2 = show $ solveP2 nums
    putStrLn ("Part 1: " ++ p1 ++ "\r\n" ++ "Part 2: " ++ p2)
