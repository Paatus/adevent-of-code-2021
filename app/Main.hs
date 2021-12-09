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
        _ -> putStrLn "That day is not yet solved :<"
