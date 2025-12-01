module Main where

parse :: String -> [Int]
parse = map rotationToInt . lines
  where
    rotationToInt :: String -> Int
    rotationToInt [] = undefined
    rotationToInt (x:xs)
      | x == 'R' = read xs
      | otherwise = read $ '-' : xs

main :: IO ()
main = interact ((\s -> s ++ "\n") . show . length . filter (== 0) . (scanl (\x y -> (x + y) `mod` 100) 50) . parse)
