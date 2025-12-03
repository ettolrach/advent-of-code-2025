module Main where

import Data.Function ((&))
import Data.List (singleton)

type Battery = Int
type Bank = [Battery]

parse :: String -> [Bank]
parse = map (map (read . singleton)) . lines

-- Turn list of nums to num. E.g. [9, 4, 2] -> 942.
undigit :: Num a => Show a => [a] -> a
undigit = go 0 . reverse
  where
    go :: Num a => Int -> [a] -> a
    go _ [] = 0
    go k (x:xs) = x * (10 ^ k) + go (k + 1) xs

-- For each i in 11..0, find largest leftmost digit which is more than i digits to the left.
findMaxJoltage :: Bank -> Int
findMaxJoltage l = undigit $ go l (reverse [0..11])
  where
    go :: Bank -> [Int] -> [Int]
    go l [] = []
    go l (x:xs) = l !! largest : go (take largest l) xs
      where
        largest = x + indexOfLargest thisList
        thisList = drop x l

indexOfLargest :: [Int] -> Int
indexOfLargest [] = 0
indexOfLargest l = go l 0 0 0
  where
    -- list -> current max candidate -> current index -> current max candidate's index.
    go :: [Int] -> Int -> Int -> Int -> Int
    go [] k _ maxIndex = maxIndex
    go (x:xs) k index maxIndex
      | x >= k = go xs x (index + 1) index
      | otherwise = go xs k (index + 1) maxIndex

main :: IO ()
main = interact ((++ "\n") . show . sum . map (findMaxJoltage . reverse) . parse)
