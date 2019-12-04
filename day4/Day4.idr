input : (Int, Int)
input = (387638,919123)

nums : List (Int, Int, Int, Int, Int, Int)
nums = [ (x,y,z,t,u,v) | x <- [3 .. 9]
                       , y <- [x .. 9]
                       , z <- [y .. 9]
                       , t <- [z .. 9]
                       , u <- [t .. 9]
                       , v <- [u .. 9] ]

valid : (Int, Int, Int, Int, Int, Int) -> Bool
valid (a, b, c, d, e, f) =
  let num = a * 100000 + b * 10000 + c * 1000 + d * 100 + e * 10 + f
  in num >= fst input && num <= snd input &&
    (a == b || b == c || c == d || d == e || e == f)


main : IO ()
main = putStrLn $ show $ length $ filter valid nums
