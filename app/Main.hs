module Main where

import Day1
import Day2
import Day3

main :: IO ()
main = do
    putStrLn "What day?"
    number <- getLine
    case read number of
        1 -> Day1.solve
        2 -> Day2.solve
        3 -> Day3.solve
