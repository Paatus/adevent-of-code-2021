module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6

main :: IO ()
main = do
    putStrLn "What day?"
    number <- getLine
    case read number of
        1 -> Day1.solve
        2 -> Day2.solve
        3 -> Day3.solve
        4 -> Day4.solve
        5 -> Day5.solve
        6 -> Day6.solve
