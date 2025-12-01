module Main where

type Position = Int
type Zeros = Int

parse :: String -> [Int]
parse = map rotationToInt . lines
  where
    rotationToInt :: String -> Int
    rotationToInt [] = undefined
    rotationToInt (x:xs)
      | x == 'R' = read xs
      | otherwise = read $ '-' : xs

calculateRotation :: (Position, Zeros) -> Int -> (Position, Zeros)
calculateRotation (pos, zeros) x = (newPos, zeros + extraZeros)
  where
    newPos = (pos + x) `mod` 100
    extraZeros =
      if x >= 0
      then (pos + x) `quot` 100
      else abs ((pos + x) `quot` 100) + fromEnum (pos /= 0 && (pos + x) <= 0)

main :: IO ()
main = interact ((\s -> s ++ "\n") . show . snd . foldl' calculateRotation (50, 0) . parse)
