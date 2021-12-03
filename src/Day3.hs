module Day3 where

testInput = [
    "00100",
    "11110",
    "10110",
    "10111",
    "10101",
    "01111",
    "00111",
    "11100",
    "10000",
    "11001",
    "00010",
    "01010"
    ]

getPowerRates :: [String] -> (Int, Int)
getPowerRates input = (gamma, epsilon)
    where gammaBits = map getMostCommon $ getNums input []
          epsilon = evalBinaryString $ map flipBit gammaBits
          gamma = evalBinaryString gammaBits

getLifeSupportRating :: [String] -> (Int, Int)
getLifeSupportRating input = (oxygen, co2)
    where oxygen = evalBinaryString $ getOxygenRating input
          co2 = evalBinaryString $ getCO2Rating input

getNums :: [String] -> [String] -> [String]
getNums ([]:_) acc = acc
getNums vals acc = getNums (map (drop 1) vals) (acc ++ [map head vals])

getMostCommon :: [Char] -> Char
getMostCommon vals =
    if ones > zeroes then
        '1'
    else
        '0'
        where ones = length $ filter (== '1') vals
              zeroes = length $ filter (== '0') vals

evalBinaryString :: String -> Int
evalBinaryString s = sum vals
    where zipped = zip (iterate (*2) 1) (reverse s)
          vals = map fst $ filter (\(x,y) -> y /= '0') zipped

flipBit :: Char -> Char
flipBit c = if c == '1' then
        '0'
    else
        '1'

mostCommonOxygen :: [Char] -> Char
mostCommonOxygen vals =
    if ones >= zeroes then
        '1'
    else
        '0'
        where ones = length $ filter (== '1') vals
              zeroes = length $ filter (== '0') vals

mostCommonCO2 :: [Char] -> Char
mostCommonCO2 vals =
    if ones < zeroes then
        '1'
    else
        '0'
        where ones = length $ filter (== '1') vals
              zeroes = length $ filter (== '0') vals


getOxygenRating = getRating mostCommonOxygen 0
getCO2Rating = getRating mostCommonCO2 0

getRating :: ([Char] -> Char) -> Int -> [String] -> String
getRating _ _ (r:[]) = r
getRating f index input =
    let keepNum = flippedIndex index $ map f $ getNums input []
        nextInput = filter (\s -> (s !! index) == keepNum) input
        flippedIndex = flip (!!)
    in
        getRating f (index + 1) nextInput

solveP1 :: [String] -> Int
solveP1 input = gamma * epsilon
    where (gamma, epsilon) = getPowerRates input

solveP2 :: [String] -> Int
solveP2 input = oxygen * co2
    where (oxygen, co2) = getLifeSupportRating input


solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day3.txt"
    let input = lines fileContents
        p1 = solveP1 input
        p2 = solveP2 input
    putStrLn ("Part 1: " ++ show p1 ++ "\n" ++ "Part 2: " ++ show p2)
