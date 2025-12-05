module Part2

import Data.List
import Data.String
import Data.List1
import Data.List.Quantifiers
import Data.SortedSet
import Decidable.Equality
import Decidable.Decidable
import System.File.ReadWrite
import System.File.Virtual

IngredientId : Type
IngredientId = Nat

record IdRange where
  constructor Range
  low, high : IngredientId

rangeFromList : List1 Nat -> IdRange
rangeFromList (x ::: y :: []) = Range x y
rangeFromList _ = assert_total $ idris_crash "Parse error"

mergeRanges : List IdRange -> List IdRange
mergeRanges [] = []
mergeRanges (x :: []) = x :: []
mergeRanges ((Range low1 high1) :: (Range low2 high2) :: xs)
  = (if high1 > low2 || high2 > low1
    then (Range low high) :: []
    else (Range low1 high1) :: (Range low2 high2) :: [])
    ++ mergeRanges xs
  where
    low = if low2 > low1 then low1 else low2
    high = if high1 < high2 then high2 else high1

-- This would be a correct solution, but sadly this crashes Idris :( so we need to optimise.
natsBetween : IdRange -> List IngredientId
natsBetween (Range low high) = go low
  where
    go : (curr : IngredientId) -> List IngredientId
    go curr = if curr == high then high :: [] else curr :: go (S curr)

solve : List IdRange -> Nat
solve rs = length . Prelude.toList . Data.SortedSet.fromList $ (mergeRanges rs) >>= natsBetween

parse : String -> List IdRange
parse s = map (rangeFromList . map cast . split (== '-')) rangesList
  where
    splitInput : List1 (List String)
    splitInput = (split (== "") . lines) s
    rangesList : List String
    rangesList = .head splitInput

getInput : IO String
getInput = fRead stdin >>= \res => case res of
  Left err => assert_total $ (idris_crash . show) err
  Right input => pure input

main : IO ()
main = do
  input <- getInput
  let parsed = parse input
  let solution = solve parsed
  printLn solution
