module GammaBoy.Types where

import Control.Monad.State.Strict
import Data.Array.IO
import Data.IORef
import Data.Word
import Data.Int
import Data.Word.Odd (Word3)
import Data.ByteString

type D3 = Word3
type A8 = Word8
type D8 = Word8
type S8 = Int8
type A16 = Word16
type D16 = Word16
type S16 = Int16
type Opcode = Word8

type GB a = StateT Mem IO a

data Mem = Mem
  { ram :: IOUArray A16 D8
  , regs :: IOArray Int D8
  }

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

data RA -- Restart Address
  = RA_00h
  | RA_08h
  | RA_10h
  | RA_18h
  | RA_20h
  | RA_28h
  | RA_30h
  | RA_38h
  deriving (Bounded, Enum, Eq, Show)

data Inst
  = LD_r8_r8
  | LD_r8_ihl
  | LD_ihl_r8
  | LD_ihl_d8
  | LD_a_idr
  | LD_a_a16
  | LD_a_d8
  | LD_idr_a
  | LD_a16_a
  | LD_a_idr_c
  | LD_idr_c_a
  | LDH_a8_a
  | LDH_a_a8
  | LD_r16_d16
  | LD_sp_hl
  | LDHL_sp_s8
  | LD_a16_sp
  | PUSH_r16
  | POP_r16
  | ADD_a_r8
  | ADD_a_ihl
  | ADD_a_d8
  | ADC_a_r8
  | ADC_a_ihl
  | ADC_a_d8
  | SUB_a_r8
  | SUB_a_ihl
  | SUB_a_d8
  | SBC_a_r8
  | SBC_a_ihl
  | SBC_a_d8
  | AND_a_r8
  | AND_a_ihl
  | AND_a_d8
  | XOR_a_r8
  | XOR_a_ihl
  | XOR_a_d8
  | OR_a_r8
  | OR_a_ihl
  | OR_a_d8
  | CP_a_r8
  | CP_a_ihl
  | CP_a_d8 
  | INC_r8
  | INC_ihl
  | DEC_r8
  | DEC_ihl
  | ADD_hl_r16
  | ADD_sp_s8
  | INC_r16
  | DEC_r16
  | SWAP_r8
  | SWAP_ihl
  | DAA
  | CPL
  | CCF
  | SCF
  | NOP
  | HALT
  | STOP
  | DI
  | EI
  | RLCA
  | RLA
  | RRCA
  | RRA
  | RLC_r8
  | RLC_ihl
  | RRC_r8
  | RRC_ihl
  | RL_r8
  | RL_ihl
  | RR_r8
  | RR_ihl
  | SLA_r8
  | SLA_ihl
  | SRA_r8
  | SRA_ihl
  | SRL_r8
  | SRL_ihl
  | BIT_d3_r8
  | BIT_d3_ihl
  | RES_d3_r8
  | RES_d3_ihl
  | SET_d3_r8
  | SET_d3_ihl
  | JP_a16
  | JP_cc_a16
  | JP_ihl
  | JR_a8
  | JR_cc_a8
  | CALL_a16
  | CALL_cc_a16
  | RST_ra
  | RET
  | RET_cc
  | RETI 
  | PREFIX_CB
  deriving (Bounded, Enum, Eq, Ord, Show)

