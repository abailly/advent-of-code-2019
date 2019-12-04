groupBy : (a -> a -> Bool) -> List a -> List (List a)
groupBy _ [] = []
groupBy p list@(x :: xs) =
  let (ys, zs) = span (p x) xs in
    (x :: ys) :: groupBy p (assert_smaller list zs)

||| The group function is a special case of groupBy.
|||
||| ```idris example
||| group [1, 1, 2, 3, 3]
||| ```
|||
group : Eq a => List a -> List (List a)
group = groupBy (==)

input : (Int, Int)
input = (158126,624574)

nums : List (List Int)
nums = [ [x,y,z,t,u,v] | x <- [1 .. 6]
                       , y <- [x .. 9]
                       , z <- [y .. 9]
                       , t <- [z .. 9]
                       , u <- [t .. 9]
                       , v <- [u .. 9] ]

valid : List Int -> (Nat -> Nat -> Bool) -> Bool
valid digits op =
  let num = foldl (\ n, d => n * 10 + d) 0 digits
  in num >= fst input && num <= snd input &&
     not (isNil $ filter (op 2) $ map length $ group digits)

valid_for_part1 : List Int -> Bool
valid_for_part1 n = valid n (>=)

valid_for_part2 : List Int -> Bool
valid_for_part2 n = valid n (==)

main : IO ()
main = do
  putStrLn $ show $ length $ filter valid_for_part1 nums
  putStrLn $ show $ length $ filter valid_for_part2 nums
