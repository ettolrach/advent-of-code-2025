module Part1

import Data.List
import Data.String
import Data.List1
import Data.List.Quantifiers
import Decidable.Equality
import Decidable.Decidable
import System.File.ReadWrite
import System.File.Virtual

export infixl 3 &&&
(&&&) : (a -> b) -> (a -> c) -> (a -> (b, c))
f &&& g = \a => (f a, g a)

IngredientId : Type
IngredientId = Nat

record IdRange where
  constructor Range
  low, high : IngredientId

rangeFromList : List1 Nat -> IdRange
rangeFromList (x ::: y :: []) = Range x y
rangeFromList _ = assert_total $ idris_crash "Parse error"

isInRange : IngredientId -> IdRange -> Type
isInRange ingredient (Range low high) =
  (compare low (S ingredient) = LT, compare ingredient (S high) = LT)

data Fresh : (ing : IngredientId) -> (rs: List IdRange) -> Type where
  InRange : --(ingredient : IngredientId)
    ---> (rs : List IdRange)
    Any (isInRange ingredient) rs
    -----------------------------------
    -> Fresh ingredient rs

Uninhabited (LT = EQ) where
  uninhabited Refl impossible
Uninhabited (LT = GT) where
  uninhabited Refl impossible
Uninhabited (EQ = LT) where
  uninhabited Refl impossible
Uninhabited (EQ = GT) where
  uninhabited Refl impossible
Uninhabited (GT = LT) where
  uninhabited Refl impossible
Uninhabited (GT = EQ) where
  uninhabited Refl impossible

DecEq Ordering where
  decEq LT LT = Yes Refl
  decEq LT EQ = No absurd
  decEq LT GT = No absurd
  decEq EQ LT = No absurd
  decEq EQ EQ = Yes Refl
  decEq EQ GT = No absurd
  decEq GT LT = No absurd
  decEq GT EQ = No absurd
  decEq GT GT = Yes Refl
    
decFresh : (ingredient : IngredientId) -> (rs : List IdRange) -> Dec (Fresh ingredient rs)
decFresh ing [] = No helper
  where
    helper : Fresh ing [] -> Void
    helper (InRange (Here x)) impossible
    helper (InRange (There x)) impossible
decFresh ing ((Range low high) :: xs)
  with (decEq (compare low (S ing)) LT) | (decEq (compare ing (S high)) LT)
  _ | Yes prf1 | Yes prf2 = Yes $ InRange (Here (prf1, prf2))
  _ | Yes _ | No contra with (decFresh ing xs)
    _ | Yes (InRange prf) = Yes $ InRange (There prf)
    _ | No contra2 = No $ \(InRange prfAny) => case prfAny of
      Here (_, prf) => contra prf
      There prf => contra2 (InRange prf)
  decFresh ing ((Range low high) :: xs) | No contra | _ with (decFresh ing xs)
    _ | Yes (InRange prf) = Yes $ InRange (There prf)
    _ | No contra2 = No $ \(InRange prfAny) => case prfAny of
      Here (prf, _) => contra prf
      There prf => contra2 (InRange prf)

parse : String -> (List IdRange, List IngredientId)
parse s = (map (rangeFromList . map cast . split (== '-')) rangesList, map cast ids)
  where
    splitInput : List1 (List String)
    splitInput = (split (== "") . lines) s
    rangesList : List String
    rangesList = .head splitInput
    ids : List String
    ids with (inBounds 0 (.tail splitInput))
      _ | Yes prf = index 0 (.tail splitInput)
      _ | No contra = assert_total $ idris_crash "Failed to parse"

getInput : IO String
getInput = fRead stdin >>= \res => case res of
  Left err => assert_total $ (idris_crash . show) err
  Right input => pure input

solve : List IdRange -> List IngredientId -> Nat
solve rs ids = count id $ map (\ing => isYes $ decFresh ing rs) ids

main : IO ()
main = do
  input <- getInput
  let parsed = parse input
  let solution = (uncurry solve) parsed
  printLn solution
