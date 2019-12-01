import Data.Nat.DivMod
import Data.String
import System
import public Prelude.WellFounded

fuelRequirement : Integer -> Integer
fuelRequirement mass = mass `div` 3 - 2

||| Take elements from a `Stream` into a `List` until some predicate
||| becomes `False`.
||| Assumes there exists one element in the stream for which the predicate _is_
||| false otherwise it will diverge.
takeWhile : (a -> Bool) -> Stream a -> List a
takeWhile f (x :: rest) =
  if f x
  then x :: takeWhile f rest
  else []

fuelRequirements : Integer -> Integer
fuelRequirements = sum . List.drop 1 . takeWhile (> 0) . iterate fuelRequirement

main : IO ()
main = do
  content <- readFile "input"
  case content of
    Right input => print $ sum $ map fuelRequirements $ catMaybes $ map parsePositive $ lines input
    Left err => print err
