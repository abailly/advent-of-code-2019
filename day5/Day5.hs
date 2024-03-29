{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Control.Arrow
import Control.Monad.Trans(liftIO)
import System.Environment

class Monad m => Intcode m where
  inp :: m Integer
  out :: Integer -> m ()

  mem :: m [Integer]
  flash :: [Integer] -> m ()

  ip :: m Int
  move :: Int -> m ()
  jmp :: Int -> m ()

  load :: ArgMode -> Int -> m Integer
  load Position ip = do
    ops <- mem
    pure $ ops !! addr ops ip
  load Immediate ip = do
    ops <- mem
    pure $ addr ops ip

  store :: ArgMode -> Int -> Integer -> m [Integer]
  store Position ip res = do
    ops <- mem
    let (l,r) = splitAt (addr ops ip) ops
        ops' = l ++ (res : drop 1 r)
    flash ops'
    pure ops'

newtype VM a = VM { runVM :: StateT ([Integer], Int) IO a }
  deriving (Functor, Applicative, Monad, MonadState ([Integer], Int), MonadIO)

instance Intcode VM where
  inp        = read <$> liftIO getLine
  out        = liftIO . putStrLn . show
  mem        = fst <$> get
  ip         = snd <$> get
  move ip'   = modify (second (+ ip'))
  jmp ip     = modify (second (const ip))
  flash ops' = modify (first (const ops'))

data Result where
  Error :: String -> Result
  Stop :: [Integer] -> Result
  Cont :: Result

data ArgMode = Position | Immediate
  deriving (Enum, Show)

addr ops ip = fromInteger $ ops !! ip

argModes :: Int -> Integer -> [ArgMode]
argModes numArgs opcode =
  let modes = opcode `div` 100
      digits = toEnum . read . pure <$> show modes
  in reverse $ replicate (numArgs - length digits) Position ++ digits

threeArgOp :: (Intcode m) => Int -> (Integer -> Integer -> Integer) -> m Result
threeArgOp pc op = do
  ops <- mem
  let [mode1, mode2, mode3 ] = argModes 3 (addr ops pc)
  op1 <- load mode1 (pc + 1)
  op2 <- load mode2 (pc + 2)
  store mode3 (pc + 3) (op1 `op` op2)
  move 4
  pure Cont

input ::  (Intcode m) => Int -> m Result
input pc = do
  ops <- mem
  i <- inp
  let [mode] = argModes 1 (addr ops pc)
  store mode (pc + 1) i
  move 2
  pure Cont

output ::  (Intcode m) => Int -> m Result
output  pc = do
  ops <- mem
  let [mode] = argModes 1 (addr ops pc)
  val <- load mode (pc + 1)
  out val
  move 2
  pure Cont

jump_if :: (Intcode m) => Int -> (Integer -> Bool) -> m Result
jump_if pc cmp = do
  ops <- mem
  let [mode1, mode2] = argModes 2 (addr ops pc)
  test <- load mode1 (pc + 1)
  if cmp test
    then load mode2 (pc + 2) >>= jmp . fromIntegral
    else move 3
  pure Cont

compare_and_set :: (Intcode m) => Int -> (Integer -> Integer -> Bool) -> m Result
compare_and_set pc cmp = do
  ops <- mem
  let [mode1, mode2, mode3] = argModes 3 (addr ops pc)
  op1 <- load mode1 (pc + 1)
  op2 <- load mode2 (pc + 2)
  let set x = store mode3 (pc + 3) x
  if op1 `cmp` op2
    then set 1 >> move 4
    else set 0 >> move 4
  pure Cont

step :: (Intcode m) => m Result
step = do
  ops <- mem
  pc <- ip
  case (ops !! pc `mod` 100) of
      1 -> threeArgOp pc (+)
      2 -> threeArgOp pc (*)
      3 -> input pc
      4 -> output pc
      5 -> jump_if pc (/= 0)
      6 -> jump_if pc (== 0)
      7 -> compare_and_set pc (<)
      8 -> compare_and_set pc (==)
      99 -> pure $ Stop ops
      other -> pure $ Error $ "Uknown opcode " ++ show other

program :: (Intcode m) => m [Integer]
program = do
  res <- step
  case res of
    Stop end -> pure end
    Cont -> program
    Error err -> error err

run initial = runStateT (runVM program) (initial, 0)

sample :: [Integer]
sample = [ 1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,6,23
         , 1,23,6,27,1,13,27,31,2,13,31,35,1,5,35,39,2,39,13,43
         , 1,10,43,47,2,13,47,51,1,6,51,55,2,55,13,59,1,59,10,63
         , 1,63,10,67,2,10,67,71,1,6,71,75,1,10,75,79,1,79,9,83
         , 2,83,6,87,2,87,9,91,1,5,91,95,1,6,95,99,1,99,9,103
         , 2,10,103,107,1,107,6,111,2,9,111,115,1,5,115,119,1
         , 10,119,123,1,2,123,127,1,127,6,0,99,2,14,0,0
         ]

sample2 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,
           1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,
           999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

main :: IO ()
main = do
  [arg] <- getArgs
  inp <- read <$> (readFile arg >>= \ nums -> pure $ "[" <> nums <> "]")
  res <- run inp
  putStrLn $ show  res
