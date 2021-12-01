module Main where

import Day1

main :: IO ()
main = do
    putStrLn "What day?"
    number <- getLine
    case read number of
        1 -> Day1.solve
