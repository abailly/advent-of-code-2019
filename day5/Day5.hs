{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.State
import Control.Monad.Trans(liftIO)

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

threeArgOp :: (Intcode m) => [Integer] -> Int -> (Integer -> Integer -> Integer) -> m Result
threeArgOp ops pc op = do
  let op1 = fromInteger $ ops !! (pc + 1)
      op2 = fromInteger $ ops !! (pc + 2)
      op3 = fromInteger $ ops !! (pc + 3)
      res = (ops !! op1) `op` (ops !! op2)
      (l, r) = splitAt op3 ops
  put ((l ++ (res : drop 1 r)), pc + 4)
  pure Cont

input ::  (Intcode m) => [Integer] -> Int -> m Result
input = undefined

output ::  (Intcode m) => [Integer] -> Int -> m Result
output = undefined

step :: (Intcode m) => m Result
step = do
  (ops, pc) <- get
  case ops !! pc of
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

sample :: [Integer]
sample = [1,12,2,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,19,6,23
        ,1,23,6,27,1,13,27,31,2,13,31,35,1,5,35,39,2,39,13,43
        ,1,10,43,47,2,13,47,51,1,6,51,55,2,55,13,59,1,59,10,63
        ,1,63,10,67,2,10,67,71,1,6,71,75,1,10,75,79,1,79,9,83
        ,2,83,6,87,2,87,9,91,1,5,91,95,1,6,95,99,1,99,9,103
        ,2,10,103,107,1,107,6,111,2,9,111,115,1,5,115,119,1
        ,10,119,123,1,2,123,127,1,127,6,0,99,2,14,0,0
        ]

main :: IO ()
main = do
  res <- runStateT (runVM program) (sample, 0)
  putStrLn $ show  res
