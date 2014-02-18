module GammaBoy.Instruction (stepInstruction) where

import GammaBoy.Imports
import GammaBoy.Types
import GammaBoy.Util

----

stepInstruction :: GB ()
stepInstruction =
  do pc <- getPC
     inst <- getRam8 pc
     instructions ! inst

----

withU8 :: (U8 -> GB ()) -> GB ()
withU8 f =
  do pc <- getPC
     u <- getRam8 (pc + 1)
     f u

withU16 :: (U16 -> GB ()) -> GB ()
withU16 f =
  do pc <- getPC
     u <- getRam16 (pc + 1)
     f u

----

instructions :: Array U8 (GB ())
instructions = listArray (0x00, 0xff) $
  [ nop					-- 0x00
  , withU16 $ ld_r16_u16 BC
  , ld_idr_a IBC
  , inc_r16 BC
  , inc_r8 B				-- 0x04
  , dec_r8 B
  , withU8 $ ld_r8_u8 B
  , rlca
  , withU16 ld_a16_sp			-- 0x08
  , add_hl_r16 BC
  , ld_a_idr IBC
  , dec_r16 BC
  , inc_r8 C				-- 0x0c
  , dec_r8 C
  , withU8 $ ld_r8_u8 C
  , rrca
  , stop				-- 0x10
  , withU16 $ ld_r16_u16 DE
  , ld_idr_a IDE
  , inc_r16 DE
  , inc_r8 D				-- 0x14
  , dec_r8 D
  , withU8 $ ld_r8_u8 D
  , rla
  , withU8 $ jr_s8 . num		-- 0x18
  , add_hl_r16 DE
  , ld_a_idr IDE
  , dec_r16 DE
  , inc_r8 E				-- 0x1c
  , dec_r8 E
  , withU8 $ ld_r8_u8 E
  , rra
  , withU8 $ jr_cc_s8 CC_NZ . num	-- 0x20
  , withU16 $ ld_r16_u16 HL
  , ld_idr_a IHLP
  , inc_r16 HL
  , inc_r8 H				-- 0x24
  , dec_r8 H
  , withU8 $ ld_r8_u8 H
  , daa
  , withU8 $ jr_cc_s8 CC_Z . num	-- 0x28
  , add_hl_r16 HL
  , ld_a_idr IHLP
  , dec_r16 HL
  , inc_r8 L				-- 0x2c
  , dec_r8 L
  , withU8 $ ld_r8_u8 L
  , cpl
  , withU8 $ jr_cc_s8 CC_NC . num	-- 0x30
  , withU16 $ ld_r16_u16 SP
  , ld_idr_a IHLN
  , inc_r16 SP
  , inc_ihl 				-- 0x34
  , dec_ihl
  , withU8 $ ld_ihl_u8
  , scf
  , withU8 $ jr_cc_s8 CC_C . num	-- 0x38
  , add_hl_r16 SP
  , ld_a_idr IHLN
  , dec_r16 SP
  , inc_r8 A				-- 0x3c
  , dec_r8 A
  , withU8 $ ld_r8_u8 A
  , ccf
  , ld_r8_r8 B B			-- 0x40
  , ld_r8_r8 B C
  , ld_r8_r8 B D
  , ld_r8_r8 B E
  , ld_r8_r8 B H			-- 0x44
  , ld_r8_r8 B L
  , ld_r8_ihl B
  , ld_r8_r8 B A
  , ld_r8_r8 C B			-- 0x48
  , ld_r8_r8 C C
  , ld_r8_r8 C D
  , ld_r8_r8 C E
  , ld_r8_r8 C H			-- 0x4c
  , ld_r8_r8 C L
  , ld_r8_ihl C
  , ld_r8_r8 C A
  , ld_r8_r8 D B			-- 0x50
  , ld_r8_r8 D C
  , ld_r8_r8 D D
  , ld_r8_r8 D E
  , ld_r8_r8 D H			-- 0x54
  , ld_r8_r8 D L
  , ld_r8_ihl D
  , ld_r8_r8 D A
  , ld_r8_r8 E B			-- 0x58
  , ld_r8_r8 E C
  , ld_r8_r8 E D
  , ld_r8_r8 E E
  , ld_r8_r8 E H			-- 0x5c
  , ld_r8_r8 E L
  , ld_r8_ihl E
  , ld_r8_r8 E A
  , ld_r8_r8 H B			-- 0x60
  , ld_r8_r8 H C
  , ld_r8_r8 H D
  , ld_r8_r8 H E
  , ld_r8_r8 H H			-- 0x64
  , ld_r8_r8 H L
  , ld_r8_ihl H
  , ld_r8_r8 H A
  , ld_r8_r8 L B			-- 0x68
  , ld_r8_r8 L C
  , ld_r8_r8 L D
  , ld_r8_r8 L E
  , ld_r8_r8 L H			-- 0x6c
  , ld_r8_r8 L L
  , ld_r8_ihl L
  , ld_r8_r8 L A
  , ld_ihl_r8 B				-- 0x70
  , ld_ihl_r8 C
  , ld_ihl_r8 D
  , ld_ihl_r8 E
  , ld_ihl_r8 H				-- 0x74
  , ld_ihl_r8 L
  , halt
  , ld_ihl_r8 A
  , ld_r8_r8 A B			-- 0x78
  , ld_r8_r8 A C
  , ld_r8_r8 A D
  , ld_r8_r8 A E
  , ld_r8_r8 A H			-- 0x7c
  , ld_r8_r8 A L
  , ld_r8_ihl A
  , ld_r8_r8 A A
  , add_a_r8 B				-- 0x80
  , add_a_r8 C
  , add_a_r8 D
  , add_a_r8 E
  , add_a_r8 H				-- 0x84
  , add_a_r8 L
  , add_a_ihl
  , add_a_r8 A
  , adc_a_r8 B				-- 0x88
  , adc_a_r8 C
  , adc_a_r8 D
  , adc_a_r8 E
  , adc_a_r8 H				-- 0x8c
  , adc_a_r8 L
  , adc_a_ihl
  , adc_a_r8 A
  , sub_a_r8 B				-- 0x90
  , sub_a_r8 C
  , sub_a_r8 D
  , sub_a_r8 E
  , sub_a_r8 H				-- 0x94
  , sub_a_r8 L
  , sub_a_ihl
  , sub_a_r8 A
  , sbc_a_r8 B				-- 0x98
  , sbc_a_r8 C
  , sbc_a_r8 D
  , sbc_a_r8 E
  , sbc_a_r8 H				-- 0x9c
  , sbc_a_r8 L
  , sbc_a_ihl
  , sbc_a_r8 A
  , and_a_r8 B				-- 0xa0
  , and_a_r8 C
  , and_a_r8 D
  , and_a_r8 E
  , and_a_r8 H				-- 0xa4
  , and_a_r8 L
  , and_a_ihl
  , and_a_r8 A
  , xor_a_r8 B				-- 0xa8
  , xor_a_r8 C
  , xor_a_r8 D
  , xor_a_r8 E
  , xor_a_r8 H				-- 0xac
  , xor_a_r8 L
  , xor_a_ihl
  , xor_a_r8 A
  , or_a_r8 B				-- 0xb0
  , or_a_r8 C
  , or_a_r8 D
  , or_a_r8 E
  , or_a_r8 H				-- 0xb4
  , or_a_r8 L
  , or_a_ihl
  , or_a_r8 A
  , cp_a_r8 B				-- 0xb8
  , cp_a_r8 C
  , cp_a_r8 D
  , cp_a_r8 E
  , cp_a_r8 H				-- 0xbc
  , cp_a_r8 L
  , cp_a_ihl
  , cp_a_r8 A
  , ret_cc CC_NZ			-- 0xc0
  , pop_r16 BC
  , withU16 $ jp_cc_a16 CC_NZ
  , withU16 $ jp_a16
  , withU16 $ call_cc_a16 CC_NZ		-- 0xc4
  , push_r16 BC
  , withU8 $ add_a_u8
  , rst_a16 0x00
  , ret_cc CC_Z				-- 0xc8
  , ret
  , withU16 $ jp_cc_a16 CC_Z
  , withU8 $ (!) instructionsPrefixCB
  , withU16 $ call_cc_a16 CC_Z		-- 0xcc
  , withU16 $ call_a16
  , withU8 $ adc_a_u8
  , rst_a16 0x08
  , ret_cc CC_NC			-- 0xd0
  , pop_r16 DE
  , withU16 $ jp_cc_a16 CC_NC
  , failure 0xd3
  , withU16 $ call_cc_a16 CC_NC		-- 0xd4
  , push_r16 DE
  , withU8 $ sub_a_u8
  , rst_a16 0x10
  , ret_cc CC_Z				-- 0xd8
  , reti
  , withU16 $ jp_cc_a16 CC_C
  , failure 0xdb
  , withU16 $ call_a16			-- 0xdc
  , failure 0xdd
  , withU8 $ sbc_a_u8
  , rst_a16 0x18
  , withU8 $ ldh_a8_a			-- 0xe0
  , pop_r16 HL
  , ld_idrc_a
  , failure 0xe3
  , failure 0xe4			-- 0xe4
  , push_r16 HL
  , withU8 $ and_a_u8
  , withU16 $ rst_a16
  , withU8 $ add_sp_s8			-- 0xe8
  , jp_ihl
  , withU16 $ ld_a16_a
  , failure 0xeb
  , failure 0xec			-- 0xec
  , failure 0xed
  , withU8 $ xor_a_u8
  , rst_a16 0x28
  , withU8 $ ldh_a_a8			-- 0xf0
  , pop_r16 AF
  , ld_a_idrc
  , di
  , failure 0xf4			-- 0xf4
  , push_r16 AF
  , withU8 $ or_a_u8
  , rst_a16 0x30
  , withU8 $ ldhl_sp_s8 . num		-- 0xf8
  , ld_sp_hl
  , withU16 $ ld_a_a16
  , ei
  , failure 0xfc			-- 0xfc
  , failure 0xfd
  , withU8 $ cp_a_u8
  , rst_a16 0x38
  ]
  where
    failure :: U8 -> GB ()
    failure op =
      do pc <- getPC
         fail ("bad opcode " ++ show op ++ " @ address " ++ show pc)

----

instructionsPrefixCB :: Array U8 (GB ())
instructionsPrefixCB = listArray (0x00, 0xff) $
  [ rlc_r8 B		-- 0x00
  , rlc_r8 C
  , rlc_r8 D
  , rlc_r8 E
  , rlc_r8 H		-- 0x04
  , rlc_r8 L
  , rlc_ihl
  , rlc_r8 A
  , rrc_r8 B		-- 0x08
  , rrc_r8 C
  , rrc_r8 D
  , rrc_r8 E
  , rrc_r8 H		-- 0x0c
  , rrc_r8 L
  , rrc_ihl
  , rrc_r8 A
  , rl_r8 B		-- 0x10
  , rl_r8 C
  , rl_r8 D
  , rl_r8 E
  , rl_r8 H		-- 0x14
  , rl_r8 L
  , rl_ihl
  , rl_r8 A
  , rr_r8 B		-- 0x18
  , rr_r8 C
  , rr_r8 D
  , rr_r8 E
  , rr_r8 H		-- 0x1c
  , rr_r8 L
  , rr_ihl
  , rr_r8 A
  , sla_r8 B		-- 0x20
  , sla_r8 C
  , sla_r8 D
  , sla_r8 E
  , sla_r8 H		-- 0x24
  , sla_r8 L
  , sla_ihl
  , sla_r8 A
  , sra_r8 B		-- 0x28
  , sra_r8 C
  , sra_r8 D
  , sra_r8 E
  , sra_r8 H		-- 0x2c
  , sra_r8 L
  , sra_ihl
  , sra_r8 A
  , swap_r8 B		-- 0x30
  , swap_r8 C
  , swap_r8 D
  , swap_r8 E
  , swap_r8 H		-- 0x34
  , swap_r8 L
  , swap_ihl
  , swap_r8 A
  , srl_r8 B		-- 0x38
  , srl_r8 C
  , srl_r8 D
  , srl_r8 E
  , srl_r8 H		-- 0x3c
  , srl_r8 L
  , srl_ihl
  , srl_r8 A
  , bit_u3_r8 0 B	-- 0x40
  , bit_u3_r8 0 C
  , bit_u3_r8 0 D
  , bit_u3_r8 0 E
  , bit_u3_r8 0 H	-- 0x44
  , bit_u3_r8 0 L
  , bit_u3_ihl 0
  , bit_u3_r8 0 A
  , bit_u3_r8 1 B	-- 0x48
  , bit_u3_r8 1 C
  , bit_u3_r8 1 D
  , bit_u3_r8 1 E
  , bit_u3_r8 1 H	-- 0x4c
  , bit_u3_r8 1 L
  , bit_u3_ihl 1
  , bit_u3_r8 1 A
  , bit_u3_r8 2 B	-- 0x50
  , bit_u3_r8 2 C
  , bit_u3_r8 2 D
  , bit_u3_r8 2 E
  , bit_u3_r8 2 H	-- 0x54
  , bit_u3_r8 2 L
  , bit_u3_ihl 2
  , bit_u3_r8 2 A
  , bit_u3_r8 3 B	-- 0x58
  , bit_u3_r8 3 C
  , bit_u3_r8 3 D
  , bit_u3_r8 3 E
  , bit_u3_r8 3 H	-- 0x5c
  , bit_u3_r8 3 L
  , bit_u3_ihl 3
  , bit_u3_r8 3 A
  , bit_u3_r8 4 B	-- 0x60
  , bit_u3_r8 4 C
  , bit_u3_r8 4 D
  , bit_u3_r8 4 E
  , bit_u3_r8 4 H	-- 0x64
  , bit_u3_r8 4 L
  , bit_u3_ihl 4
  , bit_u3_r8 4 A
  , bit_u3_r8 5 B	-- 0x68
  , bit_u3_r8 5 C
  , bit_u3_r8 5 D
  , bit_u3_r8 5 E
  , bit_u3_r8 5 H	-- 0x6c
  , bit_u3_r8 5 L
  , bit_u3_ihl 5
  , bit_u3_r8 5 A
  , bit_u3_r8 6 B	-- 0x70
  , bit_u3_r8 6 C
  , bit_u3_r8 6 D
  , bit_u3_r8 6 E
  , bit_u3_r8 6 H	-- 0x74
  , bit_u3_r8 6 L
  , bit_u3_ihl 6
  , bit_u3_r8 6 A
  , bit_u3_r8 7 B	-- 0x78
  , bit_u3_r8 7 C
  , bit_u3_r8 7 D
  , bit_u3_r8 7 E
  , bit_u3_r8 7 H	-- 0x7c
  , bit_u3_r8 7 L
  , bit_u3_ihl 7
  , bit_u3_r8 7 A
  , res_u3_r8 0 B	-- 0x80
  , res_u3_r8 0 C
  , res_u3_r8 0 D
  , res_u3_r8 0 E
  , res_u3_r8 0 H	-- 0x84
  , res_u3_r8 0 L
  , res_u3_ihl 0
  , res_u3_r8 0 A
  , res_u3_r8 1 B	-- 0x88
  , res_u3_r8 1 C
  , res_u3_r8 1 D
  , res_u3_r8 1 E
  , res_u3_r8 1 H	-- 0x8c
  , res_u3_r8 1 L
  , res_u3_ihl 1
  , res_u3_r8 1 A
  , res_u3_r8 2 B	-- 0x90
  , res_u3_r8 2 C
  , res_u3_r8 2 D
  , res_u3_r8 2 E
  , res_u3_r8 2 H	-- 0x94
  , res_u3_r8 2 L
  , res_u3_ihl 2
  , res_u3_r8 2 A
  , res_u3_r8 3 B	-- 0x98
  , res_u3_r8 3 C
  , res_u3_r8 3 D
  , res_u3_r8 3 E
  , res_u3_r8 3 H	-- 0x9c
  , res_u3_r8 3 L
  , res_u3_ihl 3
  , res_u3_r8 3 A
  , res_u3_r8 4 B	-- 0xa0
  , res_u3_r8 4 C
  , res_u3_r8 4 D
  , res_u3_r8 4 E
  , res_u3_r8 4 H	-- 0xa4
  , res_u3_r8 4 L
  , res_u3_ihl 4
  , res_u3_r8 4 A
  , res_u3_r8 5 B	-- 0xa8
  , res_u3_r8 5 C
  , res_u3_r8 5 D
  , res_u3_r8 5 E
  , res_u3_r8 5 H	-- 0xac
  , res_u3_r8 5 L
  , res_u3_ihl 5
  , res_u3_r8 5 A
  , res_u3_r8 6 B	-- 0xb0
  , res_u3_r8 6 C
  , res_u3_r8 6 D
  , res_u3_r8 6 E
  , res_u3_r8 6 H	-- 0xb4
  , res_u3_r8 6 L
  , res_u3_ihl 6
  , res_u3_r8 6 A
  , res_u3_r8 7 B	-- 0xb8
  , res_u3_r8 7 C
  , res_u3_r8 7 D
  , res_u3_r8 7 E
  , res_u3_r8 7 H	-- 0xbc
  , res_u3_r8 7 L
  , res_u3_ihl 7
  , res_u3_r8 7 A
  , set_u3_r8 0 B	-- 0xc0
  , set_u3_r8 0 C
  , set_u3_r8 0 D
  , set_u3_r8 0 E
  , set_u3_r8 0 H	-- 0xc4
  , set_u3_r8 0 L
  , set_u3_ihl 0
  , set_u3_r8 0 A
  , set_u3_r8 1 B	-- 0xc8
  , set_u3_r8 1 C
  , set_u3_r8 1 D
  , set_u3_r8 1 E
  , set_u3_r8 1 H	-- 0xcc
  , set_u3_r8 1 L
  , set_u3_ihl 1
  , set_u3_r8 1 A
  , set_u3_r8 2 B	-- 0xd0
  , set_u3_r8 2 C
  , set_u3_r8 2 D
  , set_u3_r8 2 E
  , set_u3_r8 2 H	-- 0xd4
  , set_u3_r8 2 L
  , set_u3_ihl 2
  , set_u3_r8 2 A
  , set_u3_r8 3 B	-- 0xd8
  , set_u3_r8 3 C
  , set_u3_r8 3 D
  , set_u3_r8 3 E
  , set_u3_r8 3 H	-- 0xdc
  , set_u3_r8 3 L
  , set_u3_ihl 3
  , set_u3_r8 3 A
  , set_u3_r8 4 B	-- 0xe0
  , set_u3_r8 4 C
  , set_u3_r8 4 D
  , set_u3_r8 4 E
  , set_u3_r8 4 H	-- 0xe4
  , set_u3_r8 4 L
  , set_u3_ihl 4
  , set_u3_r8 4 A
  , set_u3_r8 5 B	-- 0xe8
  , set_u3_r8 5 C
  , set_u3_r8 5 D
  , set_u3_r8 5 E
  , set_u3_r8 5 H	-- 0xec
  , set_u3_r8 5 L
  , set_u3_ihl 5
  , set_u3_r8 5 A
  , set_u3_r8 6 B	-- 0xf0
  , set_u3_r8 6 C
  , set_u3_r8 6 D
  , set_u3_r8 6 E
  , set_u3_r8 6 H	-- 0xf4
  , set_u3_r8 6 L
  , set_u3_ihl 6
  , set_u3_r8 6 A
  , set_u3_r8 7 B	-- 0xf8
  , set_u3_r8 7 C
  , set_u3_r8 7 D
  , set_u3_r8 7 E
  , set_u3_r8 7 H	-- 0xfc
  , set_u3_r8 7 L
  , set_u3_ihl 7
  , set_u3_r8 7 A
  ]
  

----

putAFromRam8 :: A16 -> GB ()
putAFromRam8 a = putA =<< getRam8 a

putRam8FromA :: A16 -> GB ()
putRam8FromA a = putRam8 a =<< getA

offsetFF00h :: A8 -> A16
offsetFF00h a = 0xff00 + num a

----

ld :: GB a -> (a -> GB ()) -> U16 -> S8 -> GB ()
ld g p b c =
  do x <- g
     p x
     incPC b
     putCycles c

----

-- | LD r8,r8
-- | 1 byte
-- | 4 cycles
ld_r8_r8 :: R8 -> R8 -> GB ()
ld_r8_r8 r0 r1 = ld (getR8 r1) (putR8 r0) 1 4

-- | LD r8,(hl)
-- | 1 byte
-- | 8 cycles
ld_r8_ihl :: R8 -> GB ()
ld_r8_ihl r = ld getIHL (putR8 r) 1 8

-- | LD (hl),r8
-- | 1 byte
-- | 8 cycles
ld_ihl_r8 :: R8 -> GB ()
ld_ihl_r8 r = ld (getR8 r) putIHL 1 8

-- | LD (hl),u8
-- | 2 bytes
-- | 12 cycles
ld_ihl_u8 :: U8 -> GB ()
ld_ihl_u8 u = ld (return u) putIHL 2 12

-- | LD a,(r16)
-- | 1 byte
-- | 8 cycles
ld_a_idr :: IDR -> GB ()
ld_a_idr r = ld (getIDR r) putAFromRam8 1 8

-- | LD a,(a16)
-- | 3 byte
-- | 16 cycles
ld_a_a16 :: A16 -> GB ()
ld_a_a16 a = ld (return a) putAFromRam8 3 16

-- | LD r8,u8
-- | 2 byte
-- | 8 cycles
ld_r8_u8 :: R8 -> U8 -> GB ()
ld_r8_u8 r u = ld (return u) (putR8 r) 2 8

-- | LD (r16),a
-- | 1 byte
-- | 8 cycles
ld_idr_a :: IDR -> GB ()
ld_idr_a r = ld (getIDR r) putRam8FromA 1 8

-- | LD (a16),a
-- | 3 bytes
-- | 16 cycles
ld_a16_a :: A16 -> GB ()
ld_a16_a a = ld (return a) putRam8FromA 3 16

-- | LD a,(c)
-- | 2 bytes
-- | 8 cycles
ld_a_idrc :: GB ()
ld_a_idrc = ld getC (putAFromRam8 . offsetFF00h) 2 8

-- | LD (c),a
-- | 2 bytes
-- | 8 cycles
ld_idrc_a :: GB ()
ld_idrc_a = ld getC (putRam8FromA . offsetFF00h) 2 8

-- | LDH (a8),a
-- | 2 bytes
-- | 12 cycles
ldh_a8_a :: A8 -> GB ()
ldh_a8_a a = ld (return a) (putRam8FromA . offsetFF00h) 2 12

-- | LDH a,(a8)
-- | 2 bytes
-- | 12 cycles
ldh_a_a8 :: A8 -> GB ()
ldh_a_a8 a = ld (return a) (putAFromRam8 . offsetFF00h) 2 12

-- | LD r16,u16
-- | 3 bytes
-- | 12 cycles
ld_r16_u16 :: R16 -> U16 -> GB ()
ld_r16_u16 r u = ld (return u) (putR16 r) 3 12

-- | LD sp,hl
-- | 1 byte
-- | 8 cycles
ld_sp_hl :: GB ()
ld_sp_hl = ld getHL putSP 1 8

-- | LDHL sp,r8 
-- | 2 bytes
-- | 12 cycles
ldhl_sp_s8 :: S8 -> GB () -- todo
ldhl_sp_s8 s =
  do sp <- getSP
     let u = num (abs s) :: U16
         apply = if s > 0 then (+) else (-)
         res = sp `apply` u
         hf = if s > 0
                 then (low sp) `apply` u < low sp
                 else (low sp) `apply` u > low sp
         cf = if s > 0
                 then res < sp
                 else res > sp
     putFlags False False hf cf
     putHL res
     incPC 2
     putCycles 12
  where low = (.&. 0x00ff)

-- | LD (a16),sp
-- | 3 bytes
-- | 20 cycles
ld_a16_sp :: A16 -> GB ()
ld_a16_sp a = ld getSP (putRam16 a) 3 20

-----

-- | PUSH r16
-- | 1 byte
-- | 16 cycles
push_r16 :: R16 -> GB ()
push_r16 r =
  do d <- getR16 r
     putISP d
     decSP 2
     incPC 1
     putCycles 16

-- | POP r16
-- | 1 byte
-- | 12 cycles
pop_r16 :: R16 -> GB ()
pop_r16 r =
  do d <- getISP
     putR16 r d
     incSP 2
     incPC 1
     putCycles 12

----

addA :: GB U8 -> U16 -> S8 -> GB ()
addA md b c =
  do d <- md
     da <- getA
     let res  = d + da
         zf = res == 0
         cf = res < d || res < da
         hf = testBit (low d + low da) 4
     putFlags zf False hf cf
     putA res
     incPC b
     putCycles c
   where low = (.&. 0x0f)

adcA :: GB U8 -> U16 -> S8 -> GB ()
adcA md b c =
  do d <- md
     da <- getA
     cb <- fromBool <$> getCF
     let res  = d + da + cb
         zf = res == 0
         cf = res < d || res < da
         hf = testBit (low d + low da + cb) 4
     putFlags zf False hf cf
     putA res
     incPC b
     putCycles c
   where low = (.&. 0x0f)

----

-- ADD a,r8
-- 1 byte
-- 4 cycles
add_a_r8 :: R8 -> GB ()
add_a_r8 r = addA (getR8 r) 1 4

-- ADD a,(hl)
-- 1 byte
-- 8 cycles
add_a_ihl :: GB ()
add_a_ihl = addA getIHL 1 8

-- ADD a,u8
-- 2 bytes
-- 8 cycles
add_a_u8 :: U8 -> GB ()
add_a_u8 u = addA (return u) 2 8

-- ADC a,r8
-- 1 byte
-- 4 cycles
adc_a_r8 :: R8 -> GB ()
adc_a_r8 r = adcA (getR8 r) 1 4

-- ADC a,(hl)
-- 1 byte
-- 8 cycles
adc_a_ihl :: GB ()
adc_a_ihl = adcA getIHL 1 8

-- ADC a,u8
-- 2 bytes
-- 8 cycles
adc_a_u8 :: U8 -> GB ()
adc_a_u8 u = adcA (return u) 2 8

----

subA :: GB U8 -> U16 -> S8 -> GB ()
subA md b c =
  do d <- md
     da <- getA
     let res  = da - d
         zf = res == 0
         cf = da > res
         hf = low da < low d
     putFlags zf False hf cf
     putA res
     incPC b
     putCycles c
   where low = (.&. 0x0f)

sbcA :: GB U8 -> U16 -> S8 -> GB ()
sbcA md b c =
  do d <- md
     da <- getA
     cb <- fromBool <$> getCF
     let res  = da - d + cb
         zf = res == 0
         cf = da > res
         hf = (low da - low d + cb) > da
     putFlags zf False hf cf
     putA res
     incPC b
     putCycles c
   where low = (.&. 0x0f)

--

-- SUB a,r8
-- 1 bytes
-- 4 cycles
sub_a_r8 :: R8 -> GB ()
sub_a_r8 r = subA (getR8 r) 1 4

-- SUB a,(hl)
-- 1 bytes
-- 8 cycles
sub_a_ihl :: GB ()
sub_a_ihl = subA getIHL 1 8

-- SUB a,u8
-- 2 bytes
-- 8 cycles
sub_a_u8 :: U8 -> GB ()
sub_a_u8 u = subA (return u) 2 8

-- SBC a,r8
-- 1 bytes
-- 4 cycles
sbc_a_r8 :: R8 -> GB ()
sbc_a_r8 r = sbcA (getR8 r) 1 4

-- SBC a,(hl)
-- 1 bytes
-- 8 cycles
sbc_a_ihl :: GB ()
sbc_a_ihl = sbcA getIHL 1 8

-- SBC a,u8
-- 2 bytes
-- 8 cycles
sbc_a_u8 :: U8 -> GB ()
sbc_a_u8 u = sbcA (return u) 2 8

----

bitOpA :: (U8 -> U8 -> U8) -> GB U8 -> U16 -> S8 -> GB ()
bitOpA f md b c =
  do d <- md
     k <- getA
     let res = k `f` d
         zf = res == 0
         hf = True
     putFlags zf False hf False
     putA res
     incPC b
     putCycles c

----

-- AND a,r8
-- 1 bytes
-- 4 cycles
and_a_r8 :: R8 -> GB ()
and_a_r8 r = bitOpA (.&.) (getR8 r) 1 4

-- AND a,(hl)
-- 1 bytes
-- 8 cycles
and_a_ihl :: GB ()
and_a_ihl = bitOpA (.&.) getIHL 1 8

-- AND a,u8
-- 2 bytes
-- 8 cycles
and_a_u8 :: U8 -> GB ()
and_a_u8 u = bitOpA (.&.) (return u) 2 8

-- XOR a,r8
-- 1 bytes
-- 4 cycles
xor_a_r8 :: R8 -> GB ()
xor_a_r8 r = bitOpA xor (getR8 r) 1 4

-- XOR a,(hl)
-- 1 bytes
-- 8 cycles
xor_a_ihl :: GB ()
xor_a_ihl = bitOpA xor getIHL 1 8

-- XOR a,u8
-- 2 bytes
-- 8 cycles
xor_a_u8 :: U8 -> GB ()
xor_a_u8 u = bitOpA xor (return u) 2 8

-- OR a,r8
-- 1 bytes
-- 4 cycles
or_a_r8 :: R8 -> GB ()
or_a_r8 r = bitOpA (.|.) (getR8 r) 1 4

-- OR a,(hl)
-- 1 bytes
-- 8 cycles
or_a_ihl :: GB ()
or_a_ihl = bitOpA (.|.) getIHL 1 8

-- OR a,u8
-- 2 bytes
-- 8 cycles
or_a_u8 :: U8 -> GB ()
or_a_u8 u = bitOpA (.|.) (return u) 2 8

----

cpA :: GB U8 -> U16 -> S8 -> GB ()
cpA md b c =
  do d <- md
     da <- getA
     let res  = da - d
         zf = res == 0
         cf = da < d
         hf = low da < low d 
     putFlags zf False hf cf
     incPC b
     putCycles c
     where low = (.&. 0x0f)

----

-- CP a,r8
-- 1 bytes
-- 4 cycles
cp_a_r8 :: R8 -> GB ()
cp_a_r8 r = cpA (getR8 r) 1 4

-- CP a,(hl)
-- 1 bytes
-- 8 cycles
cp_a_ihl :: GB ()
cp_a_ihl = cpA getIHL 1 8
     
-- CP a,u8
-- 2 bytes
-- 8 cycles
cp_a_u8 :: U8 -> GB ()
cp_a_u8 u = cpA (return u) 2 8

----

crement :: (Bits a, Num a) => (a -> a -> a) -> GB a -> (a -> GB ()) -> U16 -> S8 -> GB ()
crement h f g b c =
  do d <- f
     cf <- getCF
     let res = d `h` 1
         zf = res == 0
         hf = testBit d 4 /= testBit res 4
     putFlags zf False hf cf
     g res
     incPC b
     putCycles c

----

-- INC r8
-- 1 byte
-- 4 cycles
inc_r8 :: R8 -> GB ()
inc_r8 r = crement (+) (getR8 r) (putR8 r) 1 4

-- INC (hl)
-- 1 byte
-- 12 cycles
inc_ihl :: GB ()
inc_ihl = crement (+) getIHL putIHL 1 12

-- INC r16
-- 1 byte
-- 8 cycles
inc_r16 :: R16 -> GB ()
inc_r16 r = crement (+) (getR16 r) (putR16 r) 1 8

-- DEC r8
-- 1 byte
-- 4 cycles
dec_r8 :: R8 -> GB ()
dec_r8 r = crement (-) (getR8 r) (putR8 r) 1 4

-- DEC (hl)
-- 1 byte
-- 12 cycles
dec_ihl :: GB ()
dec_ihl = crement (-) getIHL putIHL 1 12

-- DEC r16
-- 1 byte
-- 12 cycles
dec_r16 :: R16 -> GB ()
dec_r16 r = crement (-) (getR16 r) (putR16 r) 1 8

----

-- ADD hl,r16
-- 1 byte
-- 8 cycles
add_hl_r16 :: R16 -> GB ()
add_hl_r16 r =
  do dh <- getHL
     dr <- getR16 r
     zf <- getZF
     let res = dh + dr
         hf = testBit (low dh + low dr) 12
         cf = res < dh || res < dr
     putFlags zf False hf cf
     putHL res
     incPC 1
     putCycles 8
  where
    low = (.&. 0x0fff)

-- ADD sp,s8
-- 2 bytes
-- 16 cycles
add_sp_s8 :: U8 -> GB ()
add_sp_s8 s =
  do sp <- getSP
     let d = num (0x7f .&. s)
     if testBit s 7
        then do let res = sp - d
                    hf = low sp < d
                    cf = res > sp
                putFlags False False hf cf
                putSP res 
        else do let res = sp + d
                    hf = testBit (low sp + d) 4
                    cf = res < sp || res < d
                putFlags False False hf cf
                putSP res 
     incPC 2
     putCycles 16
  where low = (.&. 0x000f)

-- DAA
-- 1 byte
-- 4 cycles
daa :: GB ()
daa =
  do da <- getA
     let d0 =  (da `mod` 100) `div` 10
         d1 = da `mod` 10
         d = (shiftL d0 4) + d1
     putA d
     incPC 1
     putCycles 4

-- CPL
-- 1 byte
-- 4 cycles
cpl :: GB ()
cpl =
  do zf <- getZF
     cf <- getCF
     da <- getA
     putFlags zf True True cf
     putA (complement da)
     incPC 1
     putCycles 4

-- CCF
-- 1 byte
-- 4 cycles
ccf :: GB ()
ccf =
  do zf <- getZF
     cf <- getCF
     putFlags zf False False (not cf)
     incPC 1
     putCycles 4

-- SCF
-- 1 byte
-- 4 cycles
scf :: GB ()
scf =
  do zf <- getZF
     putFlags zf False False True
     incPC 1
     putCycles 4

-- NOP
-- 1 byte
-- 4 cycles
nop :: GB ()
nop = incPC 1 >> putCycles 4

-- HALT
-- 1 byte
-- 4 cycles
halt :: GB ()
halt =
  do mem <- get
     put mem { mode = LowPowerMode }

-- STOP
-- 2 bytes
-- 4 cycles
stop :: GB ()
stop =
  do mem <- get
     put mem { mode = StopMode }

-- DI
-- 1 byte
-- 4 cycles
di :: GB ()
di = disableInterrupt >> incPC 1 >> putCycles 4

-- EI
-- 1 byte
-- 4 cycles
ei :: GB ()
ei = enableInterrupt >> incPC 1 >> putCycles 4

----

mvBitsCarry :: GB U8 -> (U8 -> GB ()) -> (U8 -> Int -> U8) -> Int -> U16 -> S8 -> GB ()
mvBitsCarry md p mv test b c =
  do d <- md
     let res = mv d 1
         zf = res == 0
         cf = testBit d test
     putFlags zf False False cf
     p res
     incPC b
     putCycles c

mvBits :: GB U8 -> (U8 -> GB ()) -> (U8 -> Int -> U8) -> Int -> U16 -> S8 -> GB ()
mvBits md p mv test b c =
  do d <- md
     cb <- fromBool <$> getCF
     let res = (mv d 1) + cb
         zf = res == 0
         cf = testBit d test
     putFlags zf False False cf
     p res
     incPC b
     putCycles c

----

-- RLCA
-- 1 byte
-- 4 cycles
rlca :: GB ()
rlca = mvBitsCarry getA putA rotateL 7 1 4

-- RLA
-- 1 byte
-- 4 cycles
rla :: GB ()
rla = mvBits getA putA rotateL 7 1 4

-- RRCA
-- 1 byte
-- 4 cycles
rrca :: GB ()
rrca = mvBitsCarry getA putA rotateR 0 1 4

-- RRA
-- 1 byte
-- 4 cycles
rra :: GB ()
rra = mvBits getA putA rotateR 0 1 4

-- RLC r8
-- 2 bytes
-- 8 cycles
rlc_r8 :: R8 -> GB ()
rlc_r8 r = mvBitsCarry (getR8 r) (putR8 r) rotateL 7 2 8

-- RLC (hl)
-- 2 bytes
-- 16 cycles
rlc_ihl :: GB ()
rlc_ihl = mvBitsCarry getIHL putIHL rotateL 7 2 16

-- RL r8
-- 2 bytes
-- 8 cycles
rl_r8 :: R8 -> GB ()
rl_r8 r = mvBits (getR8 r) (putR8 r) rotateL 7 2 8

-- RL (hl)
-- 2 bytes
-- 16 cycles
rl_ihl :: GB ()
rl_ihl = mvBits getIHL putIHL rotateL 7 2 16

-- RRC r8
-- 2 bytes
-- 8 cycles
rrc_r8 :: R8 -> GB ()
rrc_r8 r = mvBitsCarry (getR8 r) (putR8 r) rotateR 0 2 8

-- RRC (hl)
-- 2 bytes
-- 16 cycles
rrc_ihl :: GB ()
rrc_ihl = mvBitsCarry getIHL putIHL rotateR 0 2 16

-- RR r8
-- 2 bytes
-- 8 cycles
rr_r8 :: R8 -> GB ()
rr_r8 r = mvBits (getR8 r) (putR8 r) rotateR 0 2 8

-- RR (hl)
-- 2 bytes
-- 8 cycles
rr_ihl :: GB ()
rr_ihl = mvBits getIHL putIHL rotateR 0 2 8

-- SLA r8
-- 2 bytes
-- 8 cycles
sla_r8 :: R8 -> GB ()
sla_r8 r = mvBits (getR8 r) (putR8 r) shiftL 7 2 8

-- SLA (hl)
-- 2 bytes
-- 8 cycles
sla_ihl :: GB ()
sla_ihl = mvBits getIHL putIHL shiftL 7 2 16

-- SRA r8
-- 2 bytes
-- 8 cycles
sra_r8 :: R8 -> GB ()
sra_r8 r = mvBits (getR8 r) (putR8 r) shiftR' 0 2 8

-- SRA (hl)
-- 2 bytes
-- 16 cycles
sra_ihl :: GB ()
sra_ihl = mvBits getIHL putIHL shiftR' 0 2 16

-- SRL r8
-- 2 bytes
-- 8 cycles
srl_r8 :: R8 -> GB ()
srl_r8 r = mvBits (getR8 r) (putR8 r) shiftR 0 2 8

-- SRL (hl)
-- 2 bytes
-- 16 cycles
srl_ihl :: GB ()
srl_ihl = mvBits getIHL putIHL shiftR 0 2 16

----

swap :: GB U8 -> (U8 -> GB ()) -> U16 -> S8 -> GB ()
swap g p b c =
  do u <- g
     let u0 = shiftR u 4
         u1 = shiftL u 4
         u' = u0 .|. u1
     p u'
     incPC b
     putCycles c

----

-- SWAP r8
-- 2 bytes
-- 8 cycles
swap_r8 :: R8 -> GB ()
swap_r8 r = swap (getR8 r) (putR8 r) 2 8

-- SWAP (hl)
-- 2 bytes
-- 16 cycles
swap_ihl :: GB ()
swap_ihl = swap getIHL putIHL 2 16

----

bitU3 :: GB U8 -> (U8 -> GB ()) -> U3 -> U16 -> S8 -> GB ()
bitU3 g p u3 b c =
  do u8 <- g
     cf <- getCF
     let res = u8 .&. (num u3)
         zf = res == 0
     putFlags zf False True cf
     p res
     incPC b
     putCycles c

----

-- BIT u3,r8
-- 2 bytes
-- 8 cycles
bit_u3_r8 :: U3 -> R8 -> GB ()
bit_u3_r8 u r = bitU3 (getR8 r) (putR8 r) u 2 8

-- BIT u3,(hl)
-- 2 bytes
-- 16 cycles
bit_u3_ihl :: U3 -> GB ()
bit_u3_ihl u = bitU3 getIHL putIHL u 2 16

----

defBit :: (U8 -> Int -> U8) -> GB U8 -> (U8 -> GB ()) -> U3 -> U16 -> S8 -> GB ()
defBit f md p n b c =
  do d <- md
     p (f d (num n))
     incPC b
     putCycles c

----

-- SET u3,r8
-- 2 bytes
-- 8 cycles
set_u3_r8 :: U3 -> R8 -> GB ()
set_u3_r8 u r= defBit setBit (getR8 r) (putR8 r) u 2 8

-- SET u3,(hl)
-- 2 bytes
-- 16 cycles
set_u3_ihl :: U3 -> GB ()
set_u3_ihl u = defBit setBit getIHL putIHL u 2 16

-- RES u3,r8
-- 2 bytes
-- 8 cycles
res_u3_r8 :: U3 -> R8 -> GB ()
res_u3_r8 u r = defBit clearBit (getR8 r) (putR8 r) u 2 8

-- RES u3,(hl)
-- 2 bytes
-- 16 cycles
res_u3_ihl :: U3 -> GB ()
res_u3_ihl u = defBit clearBit getIHL putIHL u 2 16

----

-- JP a16
-- 3 bytes
-- 16 cycles
jp_a16 :: A16 -> GB ()
jp_a16 a = putPC a >> putCycles 16

-- JP cc,a16
-- 3 bytes
-- 12 cycles on failure; 16 cycles on success
jp_cc_a16 :: CC -> A16 -> GB ()
jp_cc_a16 cc a =
  do zf <- getZF
     cf <- getCF
     let cond = case cc of
          CC_Z  -> zf
          CC_NZ -> zf
          CC_C  -> cf
          CC_NC -> cf
     if cond
        then putPC a >> putCycles 16
        else incPC 3 >> putCycles 12
 
-- JP (hl)
-- 1 byte
-- 4 cycles
jp_ihl :: GB ()
jp_ihl = getHL >>= putPC >> putCycles 4


-- JR s8 (Jump from cur addr with signed value)
-- 2 bytes
-- 12 cycles
jr_s8 :: S8 -> GB ()
jr_s8 a =
  do pc <- getPC
     let a' = num a :: S16
         pc' = num pc :: S16
     putPC (num $ pc' + a')
     putCycles 12

-- JR cc,s8 (On success, jump from cur addr with signed value)
-- 2 bytes
-- 8 cycles on failure; 12 cycles on success
jr_cc_s8 :: CC -> S8 -> GB ()
jr_cc_s8 cc a =
  do zf <- getZF
     cf <- getCF
     pc <- getPC
     let a' = num a :: S16
         pc' = num pc :: S16
         cond = case cc of
           CC_Z  -> zf
           CC_NZ -> zf
           CC_C  -> cf
           CC_NC -> cf
     if cond
        then putPC (num $ pc' + a') >> putCycles 12
        else incPC 2 >> putCycles 8

----

callA :: A16 -> S8 -> GB ()
callA a c =
  do pc <- getPC
     putISP (pc + 2)
     decSP 2
     putPC a
     putCycles c

----

-- CALL a16
-- 3 bytes
-- 24 cycles
call_a16 :: A16 -> GB ()
call_a16 a = callA a 24

-- CALL cc a16
-- 3 bytes
-- 12 cycles on failure; 20 cycles on success
call_cc_a16 :: CC -> A16 -> GB ()
call_cc_a16 cc a =
  do zf <- getZF
     cf <- getCF
     let cond = case cc of
           CC_Z  -> zf
           CC_NZ -> zf
           CC_C  -> cf
           CC_NC -> cf
     if cond
        then callA a 24
        else incPC 3 >> putCycles 12

-- RST a16, where a16 is one of {$00, $08, $18, $20, $28, $30, $38}
-- 1 byte
-- 16 cycles
rst_a16 :: A16 -> GB ()
rst_a16 a =
  do pc <- getPC
     putISP pc
     decSP 2
     putPC a
     putCycles 16

----

ret' :: S8 -> GB ()
ret' c =
  do pc <- getISP
     incSP 2
     putPC pc
     putCycles c

----

-- RET
-- 1 byte
-- 16 cycles
ret :: GB ()
ret = ret' 16

-- RET cc
-- 1 byte
-- 8 cycles on failure; 20 cycles on success
ret_cc :: CC -> GB ()
ret_cc cc =
  do zf <- getZF
     cf <- getCF
     let cond = case cc of
           CC_Z  -> zf
           CC_NZ -> zf
           CC_C  -> cf
           CC_NC -> cf
     if cond
        then ret' 20
        else incPC 1 >> putCycles 8

-- RETI
-- 1 byte
-- 16 cycles
reti :: GB ()
reti = enableInterrupt >> ret' 16

