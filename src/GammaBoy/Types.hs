module GammaBoy.Types where

import Control.Monad.State.Strict
import Data.Array.IO
import Data.IORef
import Data.Word
import Data.Word.Odd (Word3)
import Data.ByteString

type GammaBoy a = StateT Memory IO a

type D3 = Word3
type A8 = Word8
type D8 = Word8
type A16 = Word16
type D16 = Word16

data Reg8
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

data Reg16
  = AF
  | BC
  | DE
  | HL
  | SP
  | PC
  deriving (Bounded, Enum, Eq, Ord, Show)

data Flag
  = ZF -- Zero
  | NF -- Nop
  | HF -- Halt
  | CF -- Carry
  deriving (Bounded, Enum, Eq, Ord, Show)

data CC -- Call Condition Flags
  = CC_Z  -- Zero
  | CC_NZ -- Not Zero
  | CC_C  -- Carry
  | CC_NC -- No Carry
  deriving (Bounded, Enum, Eq, Show)


data Memory = Memory
  { ram :: IOUArray Word16 Word8
  , reg16 :: IOArray Word Word16
  }

data R8 -- 8-bit Register
  = R8_a
  | R8_b
  | R8_c
  | R8_d
  | R8_e
  | R8_h
  | R8_l

data IDR -- Indirect
  = IDR_bc
  | IDR_de
  | IDR_hli
  | IDR_hld

data R16 -- 16-bit Register
  = R16_bc
  | R16_de
  | R16_hl
  | R16_sp

data S16 -- 16-bit Register for Stack Operations
  = S16_af
  | S16_bc
  | S16_de
  | S16_hl

data Instr			-- cycles
  = LD_r8_r8		R8  R8	-- 4 
  | LD_r8_idr_hl	R8	-- 8
  | LD_idr_hl_r8	R8	-- 8
  | LD_idr_hl_d8	D8	-- 12
  | LD_a_idr		IDR	-- 8
  | LD_a_a16		A16	-- 16
  | LD_a_d8		D8	-- 8
  | LD_idr_a		IDR	-- 8
  | LD_a16_a		A16	-- 8
  | LD_a_idr_c			-- 8
  | LD_idr_c_a			-- 8
  | LDH_a8_a		A8	-- 12
  | LDH_a_a8		A8	-- 12
  | LD_r16_d16 		R16 D16	-- 12
  | LD_sp_hl			-- 8
  | LD_sp_hl			-- 8
  | LDHL_sp_d8		D8	-- 12
  | LD_a16_sp		A16	-- 20
  | PUSH		S16	-- 16
  | POP			S16	-- 12
  | ADD_a_r8		R8	-- 4
  | ADD_a_idr_hl		-- 8
  | ADD_a_d8		D8	-- 8
  | ADC_a_r8		R8	-- 4
  | ADC_a_idr_hl		-- 8
  | ADC_a_d8		D8	-- 8
  | SUB_a_r8		R8	-- 4
  | SUB_a_idr_hl		-- 8
  | SUB_a_d8		D8	-- 8
  | SBC_a_r8		R8	-- 4
  | SBC_a_idr_hl		-- 8
  | SBC_a_d8		D8	-- 8
  | AND_r8		R8	-- 4
  | AND_idr_hl			-- 8
  | AND_d8		D8	-- 8
  | OR_r8		R8	-- 4
  | OR_idr_hl			-- 8
  | OR_d8		D8	-- 8
  | XOR_r8		R8	-- 4
  | XOR_idr_hl			-- 8
  | XOR_d8		D8	-- 8
  | CP_r8		R8	-- 4
  | CP_idr_hl			-- 8
  | CP_d8		D8	-- 8
  | INC_r8		R8	-- 4
  | INC_idr_hl			-- 12
  | DEC_r8		R8	-- 4
  | DEC_idr_hl			-- 12
  | ADD_hl		R16	-- 8
  | ADD_sp		D8	-- 16
  | INC_r16		R16	-- 8
  | DEC_r16		R16	-- 8
  | SWAP_r8		R8	-- 8
  | SWAP_idr_hl			-- 16
  | DAA				-- 4
  | CPL				-- 4
  | CCF				-- 4
  | SCF				-- 4
  | NOP				-- 4
  | HALT			-- 4
  | STOP			-- 4
  | DI				-- 4
  | EI				-- 4
  | RLCA			-- 4
  | RLA				-- 4
  | RRCA			-- 4
  | RRA				-- 4
  | RLC_r8			-- 8
  | RLC_idr_hl			-- 16
  | RRC_r8			-- 8
  | RRC_idr_hl			-- 16
  | RR_r8			-- 8
  | RR_idr_hl			-- 16
  | SLA_r8			-- 8
  | SLA_idr_hl			-- 16
  | SRL_r8			-- 8
  | SRL_idr_hl			-- 16
  | BIT_r8		D3  R8	-- 8
  | BIT_idr_hl		D3	-- 16
  | SET_r8		D3  R8	-- 8
  | SET_idr_hl		D3	-- 16
  | RES_r8		D3  R8	-- 8
  | RES_idr_hl		D3	-- 16
  | JP_a16		D16	-- 12
  | JP_cc_d16		CC  D16 -- 12
  | JP_idr_hl			-- 4
  | JR_a8		A8	-- 8
  | JR_cc_a8		CC  A8	-- 8
  | CALL_a16		A16	-- 12
  | CALL_cc_a16		CC  A16	-- 12
  | RST			D3	-- 32
  | RET				-- 8
  | RET_cc		CC	-- 8
  | RETI			-- 8
  | PrefixCB			-- 4
  deriving (Show, Eq)

