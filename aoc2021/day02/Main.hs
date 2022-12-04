{-# LANGUAGE RecordWildCards #-}

module Main where

data Position = Position
  { x :: Int,
    depth :: Int,
    aim :: Int
  }

start :: Position
start = Position {x = 0, depth = 0, aim = 0}

data Move = Forward Int | Down Int | Up Int

parseMove :: String -> Move
parseMove s =
  let [op, rest] = words s
      distance = read rest
   in case op of
        "forward" -> Forward distance
        "down" -> Down distance
        "up" -> Up distance
        _ -> error "invalid input"

applyMove :: Position -> Move -> Position
applyMove Position {..} (Forward n) = Position {x = x + n, ..}
applyMove Position {..} (Down n) = Position {depth = depth + n, ..}
applyMove Position {..} (Up n) = Position {depth = depth - n, ..}

applyMove2 :: Position -> Move -> Position
applyMove2 Position {..} (Forward n) = Position {x = x + n, depth = depth + aim * n, ..}
applyMove2 Position {..} (Down n) = Position {aim = aim + n, ..}
applyMove2 Position {..} (Up n) = Position {aim = aim - n, ..}

parseInput :: String -> [Move]
parseInput = map parseMove . lines

part1 :: [Move] -> Int
part1 ms =
  let Position {..} = foldl applyMove start ms
   in x * depth

part2 :: [Move] -> Int
part2 ms =
  let Position {..} = foldl applyMove2 start ms
   in x * depth

processFile :: ([Move] -> Int) -> FilePath -> IO ()
processFile process fname = do
  text <- readFile fname
  let moves = map parseMove . lines $ text
  print $ process moves

main :: IO ()
main = do
  processFile part1 "day02/input.txt"
  processFile part2 "day02/input.txt"
