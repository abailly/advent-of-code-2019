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

-- | Basic instance using `stdin` and `stdout` to do I/O
instance IOM IO where
  inp = read <$> getLine
  out = putStrLn . show


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
store Position ptr res = poke ptr res
store Immediate ptr _ = fail $ "Immediate mode not supported for 'store' instruction at " <> show ptr


-- | Running status of a machine
data Status where
  -- | Something wrong happened, most probably caused by an invalid instruction
  Errored :: String -> Status
  -- | Program halted without an error
  Halted :: Status
  -- | Program is running
  Running :: Status

threeArgOp :: (Intcode m) => Arg -> Arg -> Arg -> (Integer -> Integer -> Integer) -> m ()
threeArgOp arg1 arg2 arg3 op = do
  op1 <- uncurry load arg1
  op2 <- uncurry load arg2
  uncurry store arg3 (op1 `op` op2)
  move 4

input ::  (Intcode m) => Arg -> m ()
input arg = inp >>= uncurry store arg >> move 2

output ::  (Intcode m) => Arg -> m ()
output arg = uncurry load arg >>= out >> move 2

jump_if :: (Intcode m) => Arg -> Arg -> (Integer -> Bool) -> m ()
jump_if arg1 arg2 cmp = do
  test <- uncurry load arg1
  if cmp test
    then uncurry load arg2 >>= jmp . fromIntegral
    else move 3

compare_and_set :: (Intcode m) => Arg -> Arg -> Arg -> (Integer -> Integer -> Bool) -> m ()
compare_and_set arg1 arg2 arg3 cmp = do
  op1 <- uncurry load arg1
  op2 <- uncurry load arg2
  let set = uncurry store arg3
  if op1 `cmp` op2
    then set 1 >> move 4
    else set 0 >> move 4


type Arg = (ArgMode, Int)

mkArgs :: Int -> Integer -> Int -> [Arg]
mkArgs n opcode pc =
  let modes = argModes n opcode
  in  zip modes [ pc+1 .. ]

plus :: Intcode m => Arg -> Arg -> Arg -> m ()
plus arg1 arg2 arg3 = threeArgOp arg1 arg2 arg3 (+)

mult :: Intcode m => Arg -> Arg -> Arg -> m ()
mult arg1 arg2 arg3 = threeArgOp arg1 arg2 arg3 (*)

jnz :: Intcode m => Arg -> Arg -> m ()
jnz arg1 arg2 = jump_if arg1 arg2 (/= 0)

jz :: Intcode m => Arg -> Arg -> m ()
jz arg1 arg2 = jump_if arg1 arg2 (== 0)

cmplt :: Intcode m => Arg -> Arg -> Arg -> m ()
cmplt arg1 arg2 arg3 = compare_and_set arg1 arg2 arg3 (<)

cmpeq :: Intcode m => Arg -> Arg -> Arg -> m ()
cmpeq arg1 arg2 arg3 = compare_and_set arg1 arg2 arg3 (==)

step :: (Intcode m) => Integer -> Int -> m ()
step opcode pc =
  case (opcode `mod` 100) of
    1 -> let [arg1, arg2, arg3] = mkArgs 3 opcode pc
         in plus arg1 arg2 arg3
    2 -> let [arg1, arg2, arg3] = mkArgs 3 opcode pc
         in mult arg1 arg2 arg3
    3 -> let [arg1] = mkArgs 1 opcode pc
         in input arg1
    4 -> let [arg1] = mkArgs 1 opcode pc
         in output arg1
    5 -> let [arg1, arg2] = mkArgs 2 opcode pc
         in jnz arg1 arg2
    6 -> let [arg1, arg2] = mkArgs 2 opcode pc
         in jz arg1 arg2
    7 -> let [arg1, arg2, arg3] = mkArgs 3 opcode pc
         in cmplt arg1 arg2 arg3
    8 -> let [arg1, arg2, arg3] = mkArgs 3 opcode pc
         in cmpeq arg1 arg2 arg3
    99 -> halt
    other -> do
      mem <- core
      fail $ "Uknown opcode " ++ show other ++ " ip=" ++ show pc ++ ", core dump=" ++ show mem

-- | Run a machine until it halts or errors
program :: (Intcode m) => m [Integer]
program = do
  res <- status
  case res of
    Halted      -> core
    Running     -> do
      pc <- ip
      opcode <- peek pc
      step opcode pc
      program
    Errored err -> fail err
