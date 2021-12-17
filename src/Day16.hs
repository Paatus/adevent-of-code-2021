{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day16 where

import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Debug.Trace

data Packet = Packet { version :: Int
                     , typeId :: Int
                     , value :: Maybe Int
                     , subPackets :: [Packet]
                     } deriving (Show)

hextoBinString :: Char -> String
hextoBinString c = case c of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"

hexStringToBits :: String -> String
hexStringToBits s = s >>= hextoBinString

evalBinaryString :: String -> Int
evalBinaryString s = sum vals
    where zipped = zip (iterate (*2) 1) (reverse s)
          vals = map fst $ filter (\(x,y) -> y /= '0') zipped


splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

parseLiteralValue :: String -> Int
parseLiteralValue s = evalBinaryString $ concatMap (drop 1) $ take bitsCount parts
    where parts = splitEvery 5 (drop 6 s)
          bitsCount = (+) 1 $ length $ takeWhile (\s -> head s /= '0') parts

getLiteralValueLength :: String -> Int
getLiteralValueLength s = valueLength + paddingLength
    where parts = splitEvery 5 (drop 6 s)
          bitsCount = (+) 1 $ length $ takeWhile (\s -> head s /= '0') parts
          valueLength = length $ concat $ take bitsCount parts
          paddingLength = 6

parseLiteralPacket :: String -> (Packet, String)
parseLiteralPacket s = (Packet version typeID (Just val) [], remaining)
    where (version, typeID) = getBITSParts s
          val = parseLiteralValue s
          remaining = drop (getLiteralValueLength s) s

parseOperator11Packet :: String -> (Packet, String)
parseOperator11Packet s = (Packet version typeID Nothing packets, remaining)
    where (version, typeID) = getBITSParts s
          packetCount = evalBinaryString $ take 11 $ drop 7 s
          (packets, remaining) = parseNPackets packetCount (drop 18 s) []

parseOperator15Packet :: String -> (Packet, String)
parseOperator15Packet s = (Packet version typeID Nothing packets, remaining)
    where (version, typeID) = getBITSParts s
          bitsCount = evalBinaryString $ take 15 $ drop 7 s
          packets = parsePackets (take bitsCount $ drop 22 s) []
          remaining = drop (22 + bitsCount) s

getOperatorType :: String -> Int
getOperatorType s = digitToInt $ s !! 6

parseOperatorPacket :: String -> (Packet, String)
parseOperatorPacket s = (p, remaining)
    where (version, typeID) = getBITSParts s
          lengthType = getOperatorType s
          (p, remaining) = case lengthType of
            0 -> parseOperator15Packet s
            1 -> parseOperator11Packet s

parseNPackets :: Int -> String -> [Packet] -> ([Packet], String)
parseNPackets 0 s packets = (packets, s)
parseNPackets n s acc = case typeID of
    4 -> let (packet, remainder) = parseLiteralPacket s
         in parseNPackets (n-1) remainder (acc ++ [packet])
    _ -> let (packet, remainder) = parseOperatorPacket s
         in parseNPackets (n-1) remainder (acc ++ [packet])
    where (_, typeID) = getBITSParts s

isExtraBits :: String -> Bool
isExtraBits = all (== '0')

parsePackets :: String -> [Packet] -> [Packet]
parsePackets [] acc = acc
parsePackets s acc
    | isExtraBits s = acc
    | otherwise = case typeID of
    4 -> let (packet, remainder) = parseLiteralPacket s
         in parsePackets remainder (acc ++ [packet])
    _ -> let (packet, remainder) = parseOperatorPacket s
         in parsePackets remainder (acc ++ [packet])
    where (_, typeID) = getBITSParts s


getBITSParts :: String -> (Int, Int)
getBITSParts s = case typeID of
    4 -> (version, typeID)
    _ -> (version, typeID)
    where version = evalBinaryString $ take 3 s
          typeID = evalBinaryString $ take 3 $ drop 3 s

sumVersions :: [Packet] -> Int -> Int
sumVersions [] acc = acc
sumVersions (p:ps) acc = sumVersions ps (acc + total)
    where topLevelVersion = version p
          subVersions = sumVersions (subPackets p) 0
          total = topLevelVersion + subVersions

calculatePacketValue :: Packet -> Packet
calculatePacketValue p@(Packet version typeId _value _subPackets) = Packet version typeId (Just val) subPackets
    where subPackets = map calculatePacketValue _subPackets
          val = case typeId of
            4 -> fromJust $ value p
            0 -> sum $ map (fromJust . value) subPackets
            1 -> product $ map (fromJust . value) subPackets
            2 -> minimum $ map (fromJust . value) subPackets
            3 -> maximum $ map (fromJust . value) subPackets
            5 -> (\[a,b] -> if a > b then 1 else 0) $ map (fromJust . value) subPackets
            6 -> (\[a,b] -> if a < b then 1 else 0) $ map (fromJust . value) subPackets
            7 -> (\[a,b] -> if a == b then 1 else 0) $ map (fromJust . value) subPackets

solveP1 :: String -> Int
solveP1 s = sumVersions packets 0
    where binString = hexStringToBits s
          packets = parsePackets binString []

solveP2 :: String -> Int
solveP2 s = (fromJust . value) $ head $ map calculatePacketValue packets
    where binString = hexStringToBits s
          packets = parsePackets binString []

solve :: IO ()
solve = do
    fileContents <- readFile "inputs/day16.txt"
    let input = head $ lines fileContents
        p1 = solveP1 input
        p2 = solveP2 input
    putStrLn ("Part 1: " ++ show p1 ++ "\nPart 2: " ++ show p2)
