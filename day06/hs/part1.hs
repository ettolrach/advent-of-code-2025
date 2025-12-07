module Main where

import Data.List (transpose)

type Operator = Int -> Int -> Int

sToOp :: String -> Operator
sToOp "+" = (+)
sToOp "*" = (*)
sToOp _ = error "parse error"

parse :: String -> [([Int], Operator)]
parse = map parseLine . transpose . map words . lines
  where
    parseLine :: [String] -> ([Int], Operator)
    parseLine l = ((map read . take (len - 1)) l, (sToOp . last) l)
      where
        len = length l

solve :: [([Int], Operator)] -> Int
solve = sum . map foldingFn
  where
    foldingFn :: ([Int], Operator) -> Int
    foldingFn (l, op) = foldr1 op l

main :: IO ()
main = interact ((++ "\n") . show . solve . parse)
