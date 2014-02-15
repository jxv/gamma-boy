module GammaBoy.Instruction (instructions) where

import GammaBoy.Imports
import GammaBoy.Types
import GammaBoy.Util

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
  , prefix_cb
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
    failure op =
      do pc <- getPC
         let msg = "bad opcode " ++ show op ++ " @ address " ++ show pc
         fail msg

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
ldhl_sp_s8 :: S8 -> GB () -- redo
ldhl_sp_s8 s =
  do sp <- getSP
     let d = num (0x7f .&. s)
     if s < 0
        then do let res = sp - d
                    hf = low sp < d
                    cf = res > sp
                putFlags False False hf cf
                putHL res 
        else do let res = sp + d
                    hf = testBit (low sp + d) 4
                    cf = res < sp || res < d
                putFlags False False hf cf
                putHL res 
  where low = (.&. 0x000f)

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

subA_ :: GB U8 -> GB ()
subA_ md =
  do d <- md
     da <- getA
     let res  = da - d
         zf = res == 0
         cf = da > res
         hf = low da < low d
     putFlags zf False hf cf
     putA res
   where low = (.&. 0x0f)

sbcA_ :: GB U8 -> GB ()
sbcA_ md =
  do d <- md
     da <- getA
     cb <- fromBool <$> getCF
     let res  = da - d + cb
         zf = res == 0
         cf = da > res
         hf = (low da - low d + cb) > da
     putFlags zf False hf cf
     putA res
   where low = (.&. 0x0f)

--

sub_a_r8 :: R8 -> GB ()
sub_a_r8 = subA_ . getR8

sub_a_ihl :: GB ()
sub_a_ihl = subA_ getIHL
     
sub_a_u8 :: U8 -> GB ()
sub_a_u8 = subA_ . return

sbc_a_r8 :: R8 -> GB ()
sbc_a_r8 = sbcA_ . getR8

sbc_a_ihl :: GB ()
sbc_a_ihl = sbcA_ getIHL

sbc_a_u8 :: U8 -> GB ()
sbc_a_u8 = sbcA_ . return

----


bitOpA_ :: (U8 -> U8 -> U8) -> GB U8 -> GB ()
bitOpA_ f md =
  do d <- md
     k <- getA
     let res = k `f` d
         zf = res == 0
         hf = True
     putFlags zf False hf False
     putA res

--

and_a_r8 :: R8 -> GB ()
and_a_r8 = bitOpA_ (.&.) . getR8

and_a_ihl :: GB ()
and_a_ihl = bitOpA_ (.&.) getIHL

and_a_u8 :: U8 -> GB ()
and_a_u8 = bitOpA_ (.&.) . return

xor_a_r8 :: R8 -> GB ()
xor_a_r8 = bitOpA_ xor . getR8

xor_a_ihl :: GB ()
xor_a_ihl = bitOpA_ xor getIHL

xor_a_u8 :: U8 -> GB ()
xor_a_u8 = bitOpA_ xor. return

or_a_r8 :: R8 -> GB ()
or_a_r8 = bitOpA_ (.|.) . getR8

or_a_ihl :: GB ()
or_a_ihl = bitOpA_ (.|.) getIHL

or_a_u8 :: U8 -> GB ()
or_a_u8 = bitOpA_ (.|.) . return

----


cpA_ :: GB U8 -> GB ()
cpA_ md =
  do d <- md
     da <- getA
     let res  = da - d
         zf = res == 0
         cf = da < d
         hf = low da < low d 
     putFlags zf False hf cf
     where low = (.&. 0x0f)

--

cp_a_r8 :: R8 -> GB ()
cp_a_r8 = cpA_ . getR8

cp_a_ihl :: GB ()
cp_a_ihl = cpA_ getIHL
     
cp_a_u8 :: U8 -> GB ()
cp_a_u8 = cpA_ . return

----


crement :: (Bits a, Num a) => (a -> a -> a) -> GB a -> (a -> GB ()) -> GB ()
crement h f g =
  do d <- f
     cf <- getCF
     let res = d `h` 1
         zf = res == 0
         hf = testBit d 4 /= testBit res 4
     putFlags zf False hf cf
     g res

--

inc_r8 :: R8 -> GB ()
inc_r8 r = crement (+) (getR8 r) (putR8 r) 

inc_ihl :: GB ()
inc_ihl = crement (+) getIHL putIHL

inc_r16 :: R16 -> GB ()
inc_r16 r = crement (+) (getR16 r) (putR16 r) 

dec_r8 :: R8 -> GB ()
dec_r8 r = crement (-) (getR8 r) (putR8 r) 

dec_ihl :: GB ()
dec_ihl = crement (-) getIHL putIHL

dec_r16 :: R16 -> GB ()
dec_r16 r = crement (-) (getR16 r) (putR16 r) 

----


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
  where
    low = (.&. 0x0fff)

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
  where low = (.&. 0x000f)

daa :: GB ()
daa =
  do da <- getA
     let d0 =  (da `mod` 100) `div` 10
         d1 = da `mod` 10
         d = (shiftL d0 4) + d1
     putA d

cpl :: GB ()
cpl =
  do zf <- getZF
     cf <- getCF
     da <- getA
     putFlags zf True True cf
     putA (complement da)

ccf :: GB ()
ccf =
  do zf <- getZF
     cf <- getCF
     putFlags zf False False (not cf)

scf :: GB ()
scf =
  do zf <- getZF
     putFlags zf False False True

nop :: GB ()
nop = return ()

halt :: GB ()
halt = return () -- todo

stop :: GB ()
stop = return () -- todo

di :: GB ()
di = return () -- todo

ei :: GB ()
ei = return () -- todo

----


mvBitsCarry :: GB U8 -> (U8 -> GB ()) -> (U8 -> Int -> U8) -> Int -> GB ()
mvBitsCarry md p mv test =
  do d <- md
     let res = mv d 1
         zf = res == 0
         cf = testBit d test
     putFlags zf False False cf
     p res

mvBits :: GB U8 -> (U8 -> GB ()) -> (U8 -> Int -> U8) -> Int -> GB ()
mvBits md p mv test =
  do d <- md
     cb <- fromBool <$> getCF
     let res = (mv d 1) + cb
         zf = res == 0
         cf = testBit d test
     putFlags zf False False cf
     p res

--

rlca :: GB ()
rlca = mvBitsCarry getA putA rotateL 7

rla :: GB ()
rla = mvBits getA putA rotateL 7

rrca :: GB ()
rrca = mvBitsCarry getA putA rotateR 0

rra :: GB ()
rra = mvBits getA putA rotateR 0

rlc_r8 :: R8 -> GB ()
rlc_r8 r = mvBitsCarry (getR8 r) (putR8 r) rotateL 7

rlc_ihl :: GB ()
rlc_ihl = mvBitsCarry getIHL putIHL rotateL 7

rl_r8 :: R8 -> GB ()
rl_r8 r = mvBits (getR8 r) (putR8 r) rotateL 7

rl_ihl :: GB ()
rl_ihl = mvBits getIHL putIHL rotateL 7

rrc_r8 :: R8 -> GB ()
rrc_r8 r = mvBitsCarry (getR8 r) (putR8 r) rotateR 0

rrc_ihl :: GB ()
rrc_ihl = mvBitsCarry getIHL putIHL rotateR 0

rr_r8 :: R8 -> GB ()
rr_r8 r = mvBits (getR8 r) (putR8 r) rotateR 0

rr_ihl :: GB ()
rr_ihl = mvBits getIHL putIHL rotateR 0

sla_r8 :: R8 -> GB ()
sla_r8 r = mvBits (getR8 r) (putR8 r) shiftL 7

sla_ihl :: GB ()
sla_ihl = mvBits getIHL putIHL shiftL 7

sra_r8 :: R8 -> GB ()
sra_r8 r = mvBits (getR8 r) (putR8 r) shiftR' 0

sra_ihl :: GB ()
sra_ihl = mvBits getIHL putIHL shiftR' 0

srl_r8 :: R8 -> GB ()
srl_r8 r = mvBits (getR8 r) (putR8 r) shiftR 0

srl_ihl :: GB ()
srl_ihl = mvBits getIHL putIHL shiftR 0

swap_r8 :: R8 -> GB ()
swap_r8 r = return () -- todo

swap_ihl :: GB ()
swap_ihl = return () -- todo

----


bitD3 :: GB U8 -> (U8 -> GB ()) -> U3 -> GB ()
bitD3 g p d3 =
  do d8 <- g
     cf <- getCF
     let res = d8 .&. (num d3)
         zf = res == 0
     putFlags zf False True cf
     p res

--

bit_u3_r8 :: U3 -> R8 -> GB ()
bit_u3_r8 d r = bitD3 (getR8 r) (putR8 r) d

bit_u3_ihl :: U3 -> GB ()
bit_u3_ihl = bitD3 getIHL putIHL

----


defBit :: (U8 -> Int -> U8) -> GB U8 -> (U8 -> GB ()) -> U3 -> GB ()
defBit f md p n =
  do d <- md
     p (f d (num n))

--

set_r8 :: R8 -> U3 -> GB ()
set_r8 r = defBit setBit (getR8 r) (putR8 r) 

set_ihl :: U3 -> GB ()
set_ihl = defBit setBit getIHL putIHL

res_r8 :: R8 -> U3 -> GB ()
res_r8 r = defBit clearBit (getR8 r) (putR8 r) 

res_ihl :: U3 -> GB ()
res_ihl = defBit clearBit getIHL putIHL

----

jp_a16 :: A16 -> GB ()
jp_a16 = putPC

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
        then putPC a >> putCycles 12
        else incPC 3 >> putCycles 16
 

jp_ihl :: GB ()
jp_ihl = putPC =<< getHL

--

jr_s8 :: S8 -> GB ()
jr_s8 a =
  do pc <- getPC
     let a' = num a :: S16
         pc' = num pc :: S16
     putPC (num $ pc' + a') >> putCycles 12

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
        then putPC (num $ pc' + a') >> putCycles 8
        else incPC 2 >> putCycles 12

----

callA :: A16 -> GB ()
callA a =
  do pc <- getPC
     putISP (pc + 2)
     decSP 2
     putPC a

--

-- CALL a16
-- 12 cycles
-- 3 bytes
call_a16 :: A16 -> GB ()
call_a16 = callA

-- CALL cc a16
-- 12 cycles
-- 3 bytes
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
        then callA a
        else decPC 2

-- RST a16, where a16 is one of {$00, $08, $18, $20, $28, $30, $38}
-- _ cycles
-- 1 byte
rst_a16 :: A16 -> GB ()
rst_a16 a =
  do pc <- getPC
     putISP pc
     decSP 2
     putPC a

----

--

-- RET
-- 16 cycles
-- 1 byte
ret :: GB ()
ret =
  do pc <- getISP
     putPC pc
     incSP 2

-- RET cc
-- 16 cycles
-- 1 byte
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
        then ret
        else incPC 2


-- RETI
-- 16 cycles
-- 1 byte
reti :: GB ()
reti =
  do pc <- getISP
     putPC pc
     incSP 2
     ei

-- PREFIX CB
-- 1 byte
-- 4 cycles
prefix_cb :: GB ()
prefix_cb = return () -- todo


