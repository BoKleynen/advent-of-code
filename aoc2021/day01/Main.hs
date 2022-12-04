module Main where

import Control.Arrow ((&&&), (>>>))

type Depth = Int

parseInput :: String -> [Depth]
parseInput = map read . lines

windows :: [a] -> [(a, a)]
windows = zip <*> tail

windows3 :: [a] -> [(a, a, a)]
windows3 = zip3 <*> tail <*> (tail . tail)

part1 :: [Depth] -> Int
part1 = length . filter (uncurry (<)) . windows

part2 :: [Depth] -> Int
part2 = part1 . map (\(x, y, z) -> x + y + z) . windows3

main :: IO ()
main = readFile "day01/input.txt" >>= print . (part1 &&& part2) . parseInput
