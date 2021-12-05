module Utils where

splitBy     :: Char -> String -> [String]
splitBy c s =  case dropWhile (== c) s of
                      "" -> []
                      s' -> w : splitBy c s''
                            where (w, s'') = break (== c) s'
