module GammaBoy.Types where

import Control.Monad.State.Strict
import Data.Array.IO
import Data.IORef
import Data.Word
import Data.ByteString

type GammaBoy a = StateT Memory IO a

data Reg8
  = A | F 
  | B | C
  | D | E
  | H | L
  deriving (Bounded, Enum, Eq, Show)

data Reg16
  = AF
  | BC
  | DE
  | HL
  deriving (Bounded, Enum, Eq, Show)

data Flag
  = ZFlag
  | NFlag
  | HFlag
  | CFlag
  deriving (Bounded, Enum, Eq, Show)

data Address
  = ProgramCounter
  | StackPointer
  | Register Reg16
  | Constant Word16
  deriving (Eq, Show)

data Memory = Memory
  { progamCounter :: IORef Word16
  , stackPointer :: IORef Word16
  , indrect :: IORef Word16
  , registers :: IOUArray Int Word8
  , ram :: IOUArray Word16 Word8
  }

data MemoryBankController
  = MemoryBankController1
  | MemoryBankController2

data Rom = Rom
  { bank          :: IOUArray Word16 Word8
  , name          :: ByteString
  }


