import System

%hide Prelude.Stream.index

data Result : Type where
  Error : String -> Result
  Stop : List Integer -> Result
  Cont : List Integer -> (i : Nat) -> Result

infixr 10 !!

partial
(!!) : List a -> (idx : Nat) -> a
(x :: xs) !! Z = x
(x :: xs) !! (S k) = xs !! k

threeArgOp : List Integer -> (pc : Nat) -> (op : Integer -> Integer -> Integer) -> Result
threeArgOp ops pc op = let op1 = fromInteger $ ops !! (pc + 1)
                           op2 = fromInteger $ ops !! (pc + 2)
                           op3 = fromInteger $ ops !! (pc + 3)
                           res = ops !! op1 `op` ops !! op2
                           (l, r) = splitAt op3 ops
                       in Cont (l ++ (res :: drop 1 r)) (pc + 4)

step : List Integer -> (k : Nat) -> Result
step ops pc with (inBounds pc ops)
   | Yes _ = case pc `index` ops of
                  1 => threeArgOp ops pc (+)
                  2 => threeArgOp ops pc (*)
                  99 => Stop ops
                  other => Error $ "Uknown opcode " ++ show other
   | No _ = Error $ "pc " ++ show pc ++ " out of bounds " ++ show (length ops)

program : List Integer -> (k : Nat) -> List Integer
program prog k =
  case step prog k of
    Stop end => end
    Cont cont i => program cont i

input : List Integer
input = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,6,23
        ,1,23,6,27,1,13,27,31,2,13,31,35,1,5,35,39,2,39,13,43
        ,1,10,43,47,2,13,47,51,1,6,51,55,2,55,13,59,1,59,10,63
        ,1,63,10,67,2,10,67,71,1,6,71,75,1,10,75,79,1,79,9,83
        ,2,83,6,87,2,87,9,91,1,5,91,95,1,6,95,99,1,99,9,103
        ,2,10,103,107,1,107,6,111,2,9,111,115,1,5,115,119,1
        ,10,119,123,1,2,123,127,1,127,6,0,99,2,14,0,0
        ]

search : Integer -> Integer -> Integer
search noun verb =
  let inp = 1 :: noun :: verb :: drop 3 input
      res = program inp 0
  in res !! 0

run : () -> (Integer, Integer) -> IO ()
run _ (n,v) = do
  if search n v == 19690720
  then do { putStrLn ("Found : " ++ show (100 * n + v)) ; exitWith ExitSuccess }
  else pure ()

main : IO ()
main =
  let inputs = [ (n, v) | n <- [ 0 .. 99 ], v <- [ 0..99 ] ]
  in foldlM run () inputs
