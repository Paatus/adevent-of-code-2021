module Main where

import Day1
import Day2
import Day3
import Day4
import Day5
import Day6
import Day7
import Day8
import Day9
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16

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
        7 -> Day7.solve
        8 -> Day8.solve
        9 -> Day9.solve
        10 -> Day10.solve
        11 -> Day11.solve
        12 -> Day12.solve
        13 -> Day13.solve
        14 -> Day14.solve
        15 -> Day15.solve
        16 -> Day16.solve
        _ -> putStrLn "That day is not yet solved :<"
