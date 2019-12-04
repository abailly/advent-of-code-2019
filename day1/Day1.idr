import Data.Nat.DivMod
import Data.String
import System
import public Prelude.Nat
import public Prelude.WellFounded

%default total

fuelRequirement : Nat -> Nat
fuelRequirement mass with (divMod mass 3)
  fuelRequirement (r + ((S (S q)) * fromInteger 4)) | (MkDivMod (S (S q)) r _) = q
  fuelRequirement _                                 | _ = 0

step : (x : Nat) -> ((y : Nat) -> Smaller y x -> List Nat) -> List Nat
step Z     _ = []
step (S k) f =
  fuel :: rest
  where
    fuel : Nat
    fuel = fuelRequirement (S k)

    rest : List Nat
    rest = f ?gas (Prelude.Nat.LT fuel (S k))


-- totalFuel : Nat -> Nat
-- totalFuel mass = sum $ List.drop 1 $ sizeRec step mass

-- ||| Take elements from a `Stream` into a `List` until some predicate
-- ||| becomes `False`.
-- ||| Assumes there exists one element in the stream for which the predicate _is_
-- ||| false otherwise it will diverge.
-- takeWhile : (a -> Bool) -> Stream a -> List a
-- takeWhile f (x :: rest) =
--   if f x
--   then x :: takeWhile f rest
--   else []

-- fuelRequirements : Nat -> List Nat
-- fuelRequirements = sum . List.drop 1 . takeWhile (> 0) . iterate fuelRequirement

-- main : IO ()
-- main = do
--   content <- readFile "input"
--   case content of
--     Right input => print $ sum $ map fuelRequirements $ catMaybes $ map parsePositive $ lines input
--     Left err => print err
