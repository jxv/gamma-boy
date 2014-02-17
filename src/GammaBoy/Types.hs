module GammaBoy.Types where

import           Control.Monad.State        (StateT(..))
import           Data.Array.IO              (IOArray, IOUArray)
import           Data.Word                  (Word8, Word16)
import           Data.Int                   (Int8, Int16)
import           Data.IORef                 (IORef)
import           Data.Word.Odd              (Word3)

type U3 = Word3
type A8 = Word8
type U8 = Word8
type S8 = Int8
type A16 = Word16
type U16 = Word16
type S16 = Int16
type Opcode = Word8

type GB a = StateT Mem IO a

data Mem = Mem
  { ram :: IOUArray A16 U8
  , regs :: IOArray Int U8
  , cycles :: IORef S8
  , ime :: Bool	-- Interrupt Master Enable Flag
  , mode :: Mode
  }

data Mode
  = SplashMode
  | NormalMode
  | StopMode
  | LowPowerMode
  deriving (Bounded, Enum, Eq, Ord, Show)

data R8
  = A
  | F
  | B
  | C
  | D
  | E
  | H
  | L
  | SP_0
  | SP_1
  | PC_0
  | PC_1
  deriving (Bounded, Enum, Eq, Ord, Show)

data R16
  = AF
  | BC
  | DE
  | HL
  | SP
  | PC
  deriving (Bounded, Enum, Eq, Ord, Show)

data IDR
  = IBC
  | IDE
  | IHLP
  | IHLN
  deriving (Bounded, Enum, Eq, Ord, Show)

data Flag
  = ZF -- Zero
  | NF -- Subtract
  | HF -- Half Carry
  | CF -- Carry
  deriving (Bounded, Enum, Eq, Ord, Show)

data CC -- Call Condition Flags
  = CC_Z  -- Zero
  | CC_NZ -- Not Zero
  | CC_C  -- Carry
  | CC_NC -- No Carry
  deriving (Bounded, Enum, Eq, Show)

