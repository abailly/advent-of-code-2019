{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Trans(liftIO)
import System.Environment

class MonadState ([Integer], Int) m => Intcode m where
  inp :: m Integer
  out :: Integer -> m ()

newtype VM a = VM { runVM :: StateT ([Integer], Int) IO a }
  deriving (Functor, Applicative, Monad, MonadState ([Integer], Int), MonadIO)

instance Intcode VM where
  inp = read <$> liftIO getLine
  out = liftIO . putStrLn . show

data Result where
  Error :: String -> Result
  Stop :: [Integer] -> Result
  Cont :: Result

data ArgMode = Position | Immediate
  deriving (Enum, Show)

load :: ArgMode -> [Integer] -> Int -> Integer
load Position ops ip = ops !! addr ops ip
load Immediate ops ip = addr ops ip

store :: ArgMode -> [Integer] -> Int -> Integer -> [Integer]
store Position ops ip res =
  let (l,r) = splitAt (addr ops ip) ops
  in l ++ (res : drop 1 r)

addr ops ip = fromInteger $ ops !! ip

argModes :: Int -> Integer -> [ArgMode]
argModes numArgs opcode =
  let modes = opcode `div` 100
      digits = toEnum . read . pure <$> show modes
  in reverse $ replicate (numArgs - length digits) Position ++ digits

threeArgOp :: (Intcode m) => [Integer] -> Int -> (Integer -> Integer -> Integer) -> m Result
threeArgOp ops pc op = do
  let [mode1, mode2, mode3 ] = argModes 3 (addr ops pc)
      op1 = load mode1 ops (pc + 1)
      op2 = load mode2 ops (pc + 2)
      res = op1 `op` op2
      ops' = store mode3 ops (pc + 3) res
  put (ops', pc + 4)
  pure Cont

input ::  (Intcode m) => [Integer] -> Int -> m Result
input ops pc = do
  i <- inp
  let [mode] = argModes 1 (addr ops pc)
      ops' = store mode ops (pc + 1) i
  put (ops', pc + 2)
  pure Cont

output ::  (Intcode m) => [Integer] -> Int -> m Result
output  ops pc = do
  let [mode] = argModes 1 (addr ops pc)
      val = load mode ops (pc + 1)
  out val
  put (ops, pc + 2)
  pure Cont

step :: (Intcode m) => m Result
step = do
  (ops, pc) <- get
  case (ops !! pc `mod` 100) of
      1 -> threeArgOp ops pc (+)
      2 -> threeArgOp ops pc (*)
      3 -> input ops pc
      4 -> output ops pc
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


main :: IO ()
main = do
  [arg] <- getArgs
  inp <- read <$> (readFile arg >>= \ nums -> pure $ "[" <> nums <> "]")
  res <- run inp
  putStrLn $ show  res
