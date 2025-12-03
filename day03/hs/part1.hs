module Main where

import Data.List (singleton, findIndex)
import Data.Maybe (fromJust)

type Battery = Int
type Bank = [Battery]

parse :: String -> [Bank]
parse = map (map (read . singleton)) . lines

findMaxJoltage :: Bank -> Maybe Int
findMaxJoltage l = do
  largest <- findLargest l
  Just $ ((l !! largest) * 10) + maximum (drop (largest + 1) l)
  where
    findLargest :: [Int] -> Maybe Int
    findLargest [] = Nothing
    findLargest l = Just $ go l 0 0 0
    go :: [Int] -> Int -> Int -> Int -> Int
    go [] k _ maxIndex = maxIndex
    -- Make sure we don't select the last one.
    go [x] k _ maxIndex = maxIndex
    go (x:xs) k index maxIndex
      | x > k = go xs x (index + 1) index
      | otherwise = go xs k (index + 1) maxIndex

main :: IO ()
main = interact ((++ "\n") . show . sum . map (fromJust . findMaxJoltage) . parse)
