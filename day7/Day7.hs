{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Fail
import Control.Monad.Reader
import Control.Concurrent.Async
import Control.Concurrent.Chan
import Control.Monad.State
import Control.Monad.Trans(liftIO)
import Control.Lens
import Data.List(permutations, sort)
import System.Environment

import Intcode

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
                       , _runStatus :: Status
                       }

$(makeLenses ''Machine)

-- | Retrieves the value at the given address
addr :: Num a => [Integer] -> Int -> a
addr ops ptr = fromInteger $ ops !! ptr

newtype VM m a = VM { runVM :: StateT Machine m a }
  deriving (Functor, Applicative, Monad, MonadState Machine, MonadTrans)

instance IOM m => IOM (VM m) where
  inp = lift inp
  out = lift . out

instance (Monad m) => MonadFail (VM m) where
  fail = error

instance (IOM m) => Intcode (VM m) where
  status       = gets _runStatus
  halt         = runStatus .= Halted
  core         = gets _ram
  peek ptr     = flip addr ptr <$> gets _ram
  poke ptr val = do
    ops <- core
    let (l,r) = splitAt (addr ops ptr) ops
        ops' = l ++ (val : drop 1 r)
    ram .= ops'

  ip           = gets _eip
  move offset  = eip %= (+ offset)
  jmp tgt      = eip .= tgt


-- | Run a program asynchronously with given input and output channels
run :: [Integer] -> Chan Integer -> Chan Integer -> IO (Async ([Integer], Machine))
run initial inchannel outchannel =
  async $ runReaderT (runIOSM (runStateT (runVM program) (Machine initial 0 Running))) (IOS inchannel outchannel)

-- | Compute thruster signal using given prog and list of settings
-- The programs are run asynchronously using ports
thrusterSignal :: [Integer] -> [Integer] -> IO Integer
thrusterSignal prog settings = do
  chans <- initialiseChannels
  progs <- startAllPhasers chans
  injectSettings chans
  startComputation chans
  void $ waitAny progs
  readChan (head chans)
  where
    initialiseChannels = forM [1 .. length settings] (const newChan)
    startAllPhasers chans = forM (zip chans $ tail (cycle chans)) $ uncurry (run prog)
    injectSettings chans = forM_ (zip settings chans) $ \ (s, inchannel) -> writeChan inchannel s
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
