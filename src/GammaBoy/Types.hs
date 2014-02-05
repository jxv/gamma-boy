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
  | NF -- Subtract
  | HF -- Half Carry
  | CF -- Carry
  deriving (Bounded, Enum, Eq, Ord, Show)

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
  deriving (Bounded, Enum, Eq, Ord, Show)

data IDR -- Indirect Address from 16-bit Register
  = IDR_bc
  | IDR_de
  | IDR_hli
  | IDR_hld
  deriving (Bounded, Enum, Eq, Ord, Show)

data R16 -- 16-bit Register
  = R16_bc
  | R16_de
  | R16_hl
  | R16_sp
  deriving (Bounded, Enum, Eq, Ord, Show)

data S16 -- 16-bit Register for Stack Operations
  = S16_af
  | S16_bc
  | S16_de
  | S16_hl
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

data Instruction		-- cycles	bytes	opcode
  = LD_r8_r8		R8  R8	-- 4 		1	40-45,47-4d,4f
				--			50-55,57-5d,5f
				--			60-65,67-6d,6f
				--			      78-7d,7f
  | LD_r8_ihl		R8	-- 8		1	46,4e,56,5e,66,6e,7e
  | LD_ihl_r8		R8	-- 8		1	70-75,77
  | LD_ihl_d8		D8	-- 12		2	36
  | LD_a_idr		IDR	-- 8		1	0a,1a,2a,3a
  | LD_a_a16		A16	-- 16		3	fa
  | LD_a_d8		D8	-- 8		1	06,0e,16,1e,26,2e
  | LD_idr_a		IDR	-- 8		1	02,12,22,32
  | LD_a16_a		A16	-- 8		3	ea
  | LD_a_idr_c			-- 8		2	f2
  | LD_idr_c_a			-- 8		2	e2
  | LDH_a8_a		A8	-- 12		2	e0	
  | LDH_a_a8		A8	-- 12		2	f0
  | LD_r16_d16 		R16 D16	-- 12		3	01,11,21,31
  | LD_sp_hl			-- 8		1	f9
  | LDHL_sp_d8		D8	-- 12		2	f8
  | LD_a16_sp		A16	-- 20		3	08
  | PUSH		S16	-- 16		1	c5,d5,e5,f5
  | POP			S16	-- 12		1	c1,d1,e1,f1
  | ADD_a_r8		R8	-- 4		1	80-85,87
  | ADD_a_ihl			-- 8		1	86
  | ADD_a_d8		D8	-- 8		2	c6
  | ADC_a_r8		R8	-- 4		1	88-8d,8f	
  | ADC_a_ihl			-- 8		1	8e
  | ADC_a_d8		D8	-- 8		2	ce
  | SUB_a_r8		R8	-- 4		1	90-95,97
  | SUB_a_ihl			-- 8		1	96
  | SUB_a_d8		D8	-- 8		2	d6
  | SBC_a_r8		R8	-- 4		1	98-9d,9f
  | SBC_a_ihl			-- 8		1	9e
  | SBC_a_d8		D8	-- 8		2	ce
  | AND_r8		R8	-- 4		1	a0-a5,a7
  | AND_ihl			-- 8		1	a6
  | AND_d8		D8	-- 8		2	e6
  | XOR_r8		R8	-- 4		1	a8-ad,af
  | XOR_ihl			-- 8		1	ae
  | XOR_d8		D8	-- 8		2	ee
  | OR_r8		R8	-- 4		1	b0-b5,b7	
  | OR_ihl			-- 8		1	b6
  | OR_d8		D8	-- 8		2	f6
  | CP_r8		R8	-- 4		1	b8-bd,bf
  | CP_ihl			-- 8		1	be
  | CP_d8		D8	-- 8		1	fe
  | INC_r8		R8	-- 4		1	04,0c,14,1c,24,2c,3c
  | INC_ihl			-- 12		1	34
  | DEC_r8		R8	-- 4		1	05,0d,15,1d,25,2d,3d
  | DEC_ihl			-- 12		1	35
  | ADD_hl		R16	-- 8		1	09,19,29,39
  | ADD_sp		D8	-- 16		1	e8
  | INC_r16		R16	-- 8		1	03,13,23,33
  | DEC_r16		R16	-- 8		1	0b,1b,2b,3b
  | DAA				-- 4		1	27
  | CPL				-- 4		1	2f
  | CCF				-- 4		1	3f
  | SCF				-- 4		1	37
  | NOP				-- 4		1	00,(d3,e3,e4,f4,db,dd,eb-ed,fc,fd)
  | HALT			-- 4		1	76
  | STOP			-- 4		1	10
  | DI				-- 4		1	f3
  | EI				-- 4		1	fb
  | RLCA			-- 4		1	07
  | RLA				-- 4		1	17
  | RRCA			-- 4		1	0f
  | RRA				-- 4		1	1f
  | RLC_r8		R8	-- 8		2	00-05,07
  | RLC_ihl			-- 16		2	06
  | RRC_r8		R8	-- 8		2	08-0d,0f
  | RRC_ihl			-- 16		2	0e
  | RL_r8		R8	-- 8		2	01-05,07
  | RL_ihl			-- 16		2	06
  | RR_r8		R8	-- 8		2	18-1d,1f
  | RR_ihl			-- 16		2	1e
  | SLA_r8		R8	-- 8		2	20-25,27
  | SLA_ihl			-- 16		2	26
  | SRA_r8		R8	-- 8		2	28-2d,2f
  | SRA_ihl			-- 16		2	2e
  | SWAP_r8		R8	-- 8		2	30-35,37
  | SWAP_ihl			-- 16		2	36
  | SRL_r8		R8	-- 8		2	38-3d,3f
  | SRL_ihl			-- 16		2	3e
  | BIT_r8		D3  R8	-- 8		2	40-45,47,48-4d,4f
				--			50-55,57,58-5d,5f
				--			60-65,67,68-6d,6f
				--			70-75,77,78-7d,7f
  | BIT_ihl		D3	-- 16		2	46,4e,56,5e,66,6e,76,7e
  | RES_r8		D3  R8	-- 8		2	80-85,87,88-8d,8f
				--			90-95,97,98-9d,9f
				--			a0-a5,a7,a8-ad,af
				--			b0-b5,b7,b8-bd,bf
  | RES_ihl		D3	-- 16		2	86,8e,96,9e,a6,ae,b6,be
  | SET_r8		D3  R8	-- 8		2	c0-c5,c7,c8-cd,cf
				--			d0-d5,d7,d8-dd,df
				--			e0-e5,e7,e8-ed,ef
				--			f0-f5,f7,f8-fd,ff
  | SET_ihl		D3	-- 16		2	86,8e,96,9e,a6,ae,b6,be
  | JP_a16		A16	-- 16		3	c3
  | JP_cc_a16		CC  A16 -- 16/12	3	c2,c2,ca,ca
  | JP_ihl			-- 4		1	e9
  | JR_a8		A8	-- 8		2	18
  | JR_cc_a8		CC  A8	-- 8		2	20,30,28,38
  | CALL_a16		A16	-- 24/12	3	cd
  | CALL_cc_a16		CC  A16	-- 24/12	3	c4,d4,cc,dc
  | RST			RA	-- 16		1	c7,d7,e7,f7,cf,df,ef,ff
  | RET				-- 16		1	c9
  | RET_cc		CC	-- 8		1	c0,d0,c8,d8
  | RETI			-- 8		1	d9
  | PREFIX_CB			-- 4		1	cb
  deriving (Show, Eq)

