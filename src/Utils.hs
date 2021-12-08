module Utils where

import qualified Data.Map as Map

splitBy     :: Char -> String -> [String]
splitBy c s =  case dropWhile (== c) s of
                      "" -> []
                      s' -> w : splitBy c s''
                            where (w, s'') = break (== c) s'

createCountMap :: (Ord a) => [a] -> Map.Map a Int
createCountMap = Map.fromListWith (+) . flip zip (repeat 1)
