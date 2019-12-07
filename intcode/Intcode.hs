{-# LANGUAGE GADTs #-}
module Intcode where

import Control.Monad.Fail
import Prelude hiding (fail)

-- | Addressing mode for opcodes' arguments
data ArgMode = Position
               -- ^The argument is a /reference/ into the program's address space
             | Immediate
               -- ^The argument is a value to be used /as is/
  deriving (Enum, Show)

-- | Compute mode of number of arguments for given `opcode`
argModes :: Int -> Integer -> [ArgMode]
argModes numArgs opcode =
  let modes = opcode `div` 100
      digits = toEnum . read . pure <$> show modes
  in take numArgs $ reverse digits <> repeat Position

-- |Interface for I/O operations
class Monad m => IOM m where

  -- | Read a single value from input port
  inp :: m Integer

  -- | Write a single value to output port
  out :: Integer -> m ()

-- |Low-level interface for /Intcode/ machine
-- All operations of the Intcode machine are built upon those
-- low-level operations (akin to a CPU's microcode?)
class (IOM m, MonadFail m) => Intcode m where

  -- * State of the machine

  -- | Reads the whole memory of the program
  core :: m [Integer]

  -- | Read a single memory location
  peek :: Int -> m Integer

  -- | Writes a memore location with the given data
  poke :: Int -> Integer -> m ()

  -- | Running status of the machine
  status :: m Status

  -- | Halt the program
  halt :: m ()

  -- * Operations on Instruction Pointer

  -- | Retrieve current instruction pointer
  ip :: m Int

  -- | Moves the instruction pointer by given offset
  move :: Int -> m ()

  -- | Jump to given instruction pointer
  jmp :: Int -> m ()

-- * Operations on data
--
-- Those operations are not primitive but they can be replaced
-- by more efficient operations depending on the implementation
-- of the machine.

-- | Retrieve the data given by argument.
-- The actual memory location read is dependent on the
-- `ArgMode` of the instruction.
load :: Intcode m => ArgMode -> Int -> m Integer
load Position ptr = peek ptr >>= peek . fromIntegral
load Immediate ptr = peek ptr

-- | Store given value at given address
-- It is an error to store a value in `Immediate` mode
store :: Intcode m => ArgMode -> Int -> Integer -> m ()
store Immediate ptr _ = fail $ "Immediate mode not supported for 'store' instruction at " <> show ptr
store Position ptr res = poke ptr res


-- | Running status of a machine
data Status where
  -- | Something wrong happened, most probably caused by an invalid instruction
  Errored :: String -> Status
  -- | Program halted without an error
  Halted :: Status
  -- | Program is running
  Running :: Status

threeArgOp :: (Intcode m) => (Integer -> Integer -> Integer) -> m ()
threeArgOp op = do
  pc <- ip
  [mode1, mode2, mode3 ] <- argModes 3 <$> peek pc
  op1 <- load mode1 (pc + 1)
  op2 <- load mode2 (pc + 2)
  store mode3 (pc + 3) (op1 `op` op2)
  move 4

input ::  (Intcode m) => m ()
input = do
  pc <- ip
  i <- inp
  [mode] <- argModes 1 <$> peek pc
  store mode (pc + 1) i
  move 2

output ::  (Intcode m) => m ()
output = do
  pc <- ip
  [mode] <- argModes 1 <$> peek pc
  val <- load mode (pc + 1)
  out val
  move 2

jump_if :: (Intcode m) => (Integer -> Bool) -> m ()
jump_if cmp = do
  pc <- ip
  [mode1, mode2] <- argModes 2 <$> peek pc
  test <- load mode1 (pc + 1)
  if cmp test
    then load mode2 (pc + 2) >>= jmp . fromIntegral
    else move 3

compare_and_set :: (Intcode m) => (Integer -> Integer -> Bool) -> m ()
compare_and_set cmp = do
  pc <- ip
  [mode1, mode2, mode3] <- argModes 3 <$> peek pc
  op1 <- load mode1 (pc + 1)
  op2 <- load mode2 (pc + 2)
  let set x = store mode3 (pc + 3) x
  if op1 `cmp` op2
    then set 1 >> move 4
    else set 0 >> move 4

step :: (Intcode m) => m ()
step = do
  opcode <- ip >>= peek
  case (opcode `mod` 100) of
      1 -> threeArgOp (+)
      2 -> threeArgOp (*)
      3 -> input
      4 -> output
      5 -> jump_if (/= 0)
      6 -> jump_if (== 0)
      7 -> compare_and_set (<)
      8 -> compare_and_set (==)
      99 -> halt
      other -> fail $ "Uknown opcode " ++ show other

-- | Run a machine until it halts or errors
program :: (Intcode m) => m [Integer]
program = do
  res <- status
  case res of
    Halted      -> core
    Running     -> step >> program
    Errored err -> fail err
