{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Reader
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.Fail
import Control.Monad.State
import Control.Arrow
import Control.Monad.Trans(liftIO)
import Control.Lens
import Data.Function
import Data.List(permutations, sort)
import Data.Ord
import System.Environment

data ArgMode = Position | Immediate
  deriving (Enum, Show)


addr ops ip = fromInteger $ ops !! ip

argModes :: Int -> Integer -> [ArgMode]
argModes numArgs opcode =
  let modes = opcode `div` 100
      digits = toEnum . read . pure <$> show modes
  in reverse $ replicate (numArgs - length digits) Position ++ digits

class Monad m => IOM m where
  inp :: m Integer
  out :: Integer -> m ()

class IOM m => Intcode m where
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


data Result where
  Error :: String -> Result
  Stop :: [Integer] -> Result
  Cont :: Result


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

-- * I/O based on channels
data IOS = IOS { _cin :: Chan Integer
               , _cout :: Chan Integer
               }

$(makeLenses ''IOS)

newtype IOSM a = IOSM { runIOSM :: ReaderT IOS IO a }
  deriving (Functor, Applicative, Monad, MonadReader IOS, MonadIO)

instance IOM IOSM where
  inp = asks _cin >>= liftIO . readChan
  out x = asks _cout >>= liftIO . flip writeChan x

-- * Core machine
data Machine = Machine { _ram :: [Integer]
                       , _eip :: Int
                       }

$(makeLenses ''Machine)

newtype VM m a = VM { runVM :: StateT Machine m a }
  deriving (Functor, Applicative, Monad, MonadState Machine, MonadTrans)

instance IOM m => IOM (VM m) where
  inp = lift inp
  out = lift . out

instance IOM m => Intcode (VM m) where
  mem        = gets _ram
  ip         = gets _eip
  move ip'   = eip %= (+ ip')
  jmp ip     = eip .= ip
  flash ops' = ram .= ops'

-- | Run a program asynchronously with given input and output channels
run :: [Integer] -> Chan Integer -> Chan Integer -> IO (Async ([Integer], Machine))
run initial cin cout =
  async $ runReaderT (runIOSM (runStateT (runVM program) (Machine initial 0))) (IOS cin cout)

-- | Compute thruster signal using given prog and list of settings
-- The programs are run asynchronously using ports
thrusterSignal :: [Integer] -> [Integer] -> IO Integer
thrusterSignal prog settings = do
  chans <- initialiseChannels
  progs <- startAllPhasers chans
  injectSettings chans
  startComputation chans
  waitAny progs
  readChan (head chans)
  where
    initialiseChannels = forM [1 .. length settings] (const newChan)
    startAllPhasers chans = forM (zip chans $ tail (cycle chans)) $ uncurry (run prog)
    injectSettings chans = forM_ (zip settings chans) $ \ (s, cin) -> writeChan cin s
    startComputation chans =  writeChan (head chans) 0

maxThrusterSettings :: [Integer] -> IO (Integer, [Integer])
maxThrusterSettings prog = do
  let ins = permutations [5,6,7,8,9]
  res <- forM ins $ \ settings -> thrusterSignal prog settings
  pure $ head $ reverse $ sort (zip res ins)

main :: IO ()
main = do
  [arg] <- getArgs
  prog <- read <$> (readFile arg >>= \ nums -> pure $ "[" <> nums <> "]")
  print =<< maxThrusterSettings prog
