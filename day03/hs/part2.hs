module Main where

import Data.List (singleton)

type Battery = Int
type Bank = [Battery]

parse :: String -> [Bank]
parse = map (map (read . singleton)) . lines

undigit :: Num a => Show a => [a] -> a
undigit = go 0 . reverse
  where
    go :: Num a => Int -> [a] -> a
    go _ [] = 0
    go k (x:xs) = x * (10 ^ k) + go (k + 1) xs

findMaxJoltage :: Bank -> Int
findMaxJoltage l = undigit $ go l (reverse [0..11])
  where
    go :: Bank -> [Int] -> [Int]
    go l [] = []
    go l (x:xs) = l !! largest : go (take largest l) xs
      where
        largest = x + findLargest thisList
        thisList = drop x l

findLargest :: [Int] -> Int
findLargest [] = 0
findLargest l = go l 0 0 0
  where
    go :: [Int] -> Int -> Int -> Int -> Int
    go [] k _ maxIndex = maxIndex
    go (x:xs) k index maxIndex
      | x >= k = go xs x (index + 1) index
      | otherwise = go xs k (index + 1) maxIndex
      
main :: IO ()
main = interact ((++ "\n") . show . sum . map (findMaxJoltage . reverse) . parse)
