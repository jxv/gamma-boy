module GammaBoy.Inst where

import Control.Monad.State.Strict
import Control.Applicative
import Data.Array.IO
import Data.IORef
import Data.Word
import Data.Word.Odd (Word3)
import Data.ByteString
import Data.Bits
import Foreign.Marshal.Utils (fromBool)
import GammaBoy.Types
import GammaBoy.Util

cycles :: (Integral a) => Inst -> a
cycles instr = case instr of
  LD_r8_r8    -> 4
  LD_r8_ihl   -> 8
  LD_ihl_r8   -> 8
  LD_ihl_d8   -> 8
  LD_a_idr    -> 8
  LD_a_a16    -> 16
  LD_a_d8     -> 8
  LD_idr_a    -> 8
  LD_a16_a    -> 8
  LD_a_idr_c  -> 8
  LD_idr_c_a  -> 8
  LDH_a8_a    -> 12
  LDH_a_a8    -> 12
  LD_r16_d16  -> 12
  LD_sp_hl    -> 8
  LDHL_sp_s8  -> 12
  LD_a16_sp   -> 20
  PUSH_r16    -> 16
  POP_r16     -> 12
  ADD_a_r8    -> 4
  ADD_a_ihl   -> 8
  ADD_a_d8    -> 8
  ADC_a_r8    -> 4
  ADC_a_ihl   -> 8
  ADC_a_d8    -> 8
  SUB_a_r8    -> 4
  SUB_a_ihl   -> 8
  SUB_a_d8    -> 8
  SBC_a_r8    -> 4
  SBC_a_ihl   -> 8
  SBC_a_d8    -> 8
  AND_a_r8    -> 4
  AND_a_ihl   -> 8
  AND_a_d8    -> 8
  XOR_a_r8    -> 4
  XOR_a_ihl   -> 8
  XOR_a_d8    -> 8
  OR_a_r8     -> 4
  OR_a_ihl    -> 8
  OR_a_d8     -> 8
  CP_a_r8     -> 4
  CP_a_ihl    -> 8
  CP_a_d8     -> 8
  INC_r8      -> 4
  INC_ihl     -> 12
  DEC_r8      -> 4
  DEC_ihl     -> 12
  ADD_hl_r16  -> 8
  ADD_sp_s8   -> 16
  INC_r16     -> 8
  DEC_r16     -> 8
  DAA         -> 4
  CPL         -> 4
  CCF         -> 4
  SCF         -> 4
  NOP         -> 4
  HALT        -> 4
  STOP        -> 4
  DI          -> 4
  EI          -> 4
  RLCA        -> 4
  RLA         -> 4
  RRCA        -> 4
  RRA         -> 4
  RLC_r8      -> 8
  RLC_ihl     -> 16
  RRC_r8      -> 8
  RRC_ihl     -> 16
  RL_r8       -> 8
  RL_ihl      -> 16
  RR_r8       -> 8
  RR_ihl      -> 16
  SLA_r8      -> 8
  SLA_ihl     -> 16
  SWAP_r8     -> 8
  SWAP_ihl    -> 16
  SRL_r8      -> 8
  SRL_ihl     -> 16
  BIT_d3_r8   -> 8
  BIT_d3_ihl  -> 16
  RES_d3_r8   -> 8
  RES_d3_ihl  -> 16
  SET_d3_r8   -> 8
  SET_d3_ihl  -> 16
  JP_a16      -> 16
  JP_cc_a16   -> 12
  CALL_a16    -> 12
  CALL_cc_a16 -> 12
  RST_ra      -> 16
  RET         -> 16
  RET_cc      -> 8
  RETI        -> 8
  PREFIX_CB   -> 4

cyclesFromCCSuccess :: (Integral a) => Inst -> a
cyclesFromCCSuccess instr = case instr of
  JP_cc_a16   -> 4
  JR_cc_a8    -> 4
  CALL_cc_a16 -> 12
  RET_cc      -> 12
  _ -> 0

--

putAFromRam8 :: A16 -> GB ()
putAFromRam8 a =
  do d <- getRam8 a
     putA d

putRam8FromA :: A16 -> GB ()
putRam8FromA a =
  do d <- getA
     putRam8 a d

--

putAFromRam8Offset :: A8 -> GB ()
putAFromRam8Offset a = putAFromRam8 (0xff00 + num a)

putRam8OffsetFromA :: A8 -> GB ()
putRam8OffsetFromA a = putRam8FromA (0xff00 + num a)

--

ld_r8_r8 :: R8 -> R8 -> GB ()
ld_r8_r8 r0 r1 =
  do d1 <- getR8 r1
     putR8 r0 d1

ld_r8_ihl :: R8 -> GB ()
ld_r8_ihl r =
  do d <- getIHL
     putR8 r d

ld_ihl_r8 :: R8 -> GB ()
ld_ihl_r8 r =
  do d <- getR8 r
     putIHL d

ld_ihl_d8 :: D8 -> GB ()
ld_ihl_d8 d = putIHL d

ld_a_idr :: R16 -> GB ()
ld_a_idr r =
  do a <- getR16 r
     putAFromRam8 a

ld_a_a16 :: A16 -> GB ()
ld_a_a16 = putAFromRam8

ld_a_d8 :: D8 -> GB ()
ld_a_d8 = putA

ld_idr_a :: R16 -> GB ()
ld_idr_a r =
  do a <- getR16 r
     putRam8FromA a

ld_a16_a :: A16 -> GB ()
ld_a16_a = putRam8FromA

ld_a_idr_c :: GB ()
ld_a_idr_c =
  do a <- getC
     putAFromRam8Offset a

ld_idr_c_a :: GB ()
ld_idr_c_a =
  do a <- getC
     putRam8OffsetFromA a

ldh_a8_a :: A8 -> GB ()
ldh_a8_a a = putRam8OffsetFromA a

ld_r16_d16 :: R16 -> D16 -> GB ()
ld_r16_d16 = putR16

ld_sp_hl :: GB ()
ld_sp_hl =
  do d <- getHL
     putSP d

ldhl_sp_s8 :: D8 -> GB ()
ldhl_sp_s8 s =
  do sp <- getSP
     let d = num (0x7f .&. s)
     if testBit s 7
        then do let res = sp - d
                    hf = (sp .&. 0x000f) < d
                    cf = res > sp
                putFlags False False hf cf
                putHL res 
        else do let res = sp + d
                    hf = testBit ((sp .&. 0x000f) + d) 4
                    cf = res < sp || res < d
                putFlags False False hf cf
                putHL res 

ld_a16_sp :: A16 -> GB ()
ld_a16_sp a =
  do sp <- getSP
     putRam16 a sp

--

push_r16 :: R16 -> GB ()
push_r16 r =
  do d <- getR16 r
     putISP d
     decSP 2

pop_r16 :: R16 -> GB ()
pop_r16 r =
  do d <- getISP
     putR16 r d
     incSP 2

----

addA_ :: D8 -> GB ()
addA_ d =
  do k <- getA
     let res  = d + k
         zf = res == 0
         cf = res < d || res < k
         hf = testBit (low d + low k) 4
     putFlags zf False hf cf
     putA res
   where low = (.&. 0x0f)

--

add_a_r8 :: R8 -> GB ()
add_a_r8 r =
  do d <- getR8 r
     addA_ d

add_a_ihl :: GB ()
add_a_ihl =
  do d <- getIHL
     addA_ d
     
add_a_d8 :: D8 -> GB ()
add_a_d8 = addA_


----

adcA_ :: D8 -> GB ()
adcA_ d =
  do da <- getA
     cb <- fromBool <$> getCF
     let res = da + d + cb
         zf = res == 0
         cf = res < da || res < d
         hf = testBit (low da + low d + cb) 4
     putFlags zf False hf cf
     putA res
   where low = (.&. 0x0f)

--

adc_a_r8 :: R8 -> GB ()
adc_a_r8 r =
  do d <- getR8 r
     adcA_ d

adc_a_ihl :: GB ()
adc_a_ihl =
  do d <- getIHL
     adcA_ d
     
adc_a_d8 :: D8 -> GB ()
adc_a_d8 = adcA_


----

subA_ :: D8 -> GB ()
subA_ d =
  do da <- getA
     let res  = da - d
         zf = res == 0
         cf = da > res
         hf = low da < low d
     putFlags zf False hf cf
     putA res
   where low = (.&. 0x0f)

--

sub_a_r8 :: R8 -> GB ()
sub_a_r8 r =
  do d <- getR8 r
     subA_ d

sub_a_ihl :: GB ()
sub_a_ihl =
  do d <- getIHL
     subA_ d
     
sub_a_d8 :: D8 -> GB ()
sub_a_d8 = subA_


----

sbcA_ :: D8 -> GB ()
sbcA_ d =
  do da <- getA
     cb <- fromBool <$> getCF
     let res  = da - d + cb
         zf = res == 0
         cf = da > res
         hf = (low da - low d + cb) > da
     putFlags zf False hf cf
     putA res
   where low = (.&. 0x0f)

--

sbc_a_r8 :: R8 -> GB ()
sbc_a_r8 r =
  do d <- getR8 r
     sbcA_ d

sbc_a_ihl :: GB ()
sbc_a_ihl =
  do d <- getIHL
     sbcA_ d
     
sbc_a_d8 :: D8 -> GB ()
sbc_a_d8 = sbcA_

----


bitOpA_ :: (D8 -> D8) -> GB ()
bitOpA_ f =
  do k <- getA
     let res = f k
         zf = res == 0
         hf = True
     putFlags zf False hf False
     putA res

--

and_a_r8 :: R8 -> GB ()
and_a_r8 r = 
  do d <- getR8 r
     bitOpA_ (.&. d)

and_a_ihl :: GB ()
and_a_ihl =
  do d <- getIHL
     bitOpA_ (.&. d)

and_a_d8 :: D8 -> GB ()
and_a_d8 = bitOpA_ . (.&.)

xor_a_r8 :: R8 -> GB ()
xor_a_r8 r =
  do d <- getR8 r
     bitOpA_ (xor d)

xor_a_ihl :: GB ()
xor_a_ihl =
  do d <- getIHL
     bitOpA_ (xor d)

xor_a_d8 :: D8 -> GB ()
xor_a_d8 = bitOpA_ . xor

or_a_r8 :: R8 -> GB ()
or_a_r8 r =
  do d <- getR8 r
     bitOpA_ (.|. d)

or_a_ihl :: GB ()
or_a_ihl =
  do d <- getIHL
     bitOpA_ (.|. d)

or_a_d8 :: D8 -> GB ()
or_a_d8 = bitOpA_ . (.|.)

----


cpA_ :: D8 -> GB ()
cpA_ d =
  do da <- getA
     let res  = da - d
         zf = res == 0
         cf = da < d
         hf = low da < low d 
     putFlags zf False hf cf
     where low = (.&. 0x0f)

--

cp_a_r8 :: R8 -> GB ()
cp_a_r8 r =
  do d <- getR8 r
     cpA_ d

cp_a_ihl :: GB ()
cp_a_ihl =
  do d <- getIHL
     cpA_ d
     
cp_a_d8 :: D8 -> GB ()
cp_a_d8 = subA_

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
    low x = x .&. 0x0fff

--

add_sp_s8 :: D8 -> GB ()
add_sp_s8 s =
  do sp <- getSP
     let d = num (0x7f .&. s)
     if testBit s 7
        then do let res = sp - d
                    hf = (sp .&. 0x000f) < d
                    cf = res > sp
                putFlags False False hf cf
                putSP res 
        else do let res = sp + d
                    hf = testBit ((sp .&. 0x000f) + d) 4
                    cf = res < sp || res < d
                putFlags False False hf cf
                putSP res 

--


daa :: GB ()
daa =
  do da <- getA
     let d0 =  (da `mod` 100) `div` 10
         d1 = da `mod` 10
         d = shiftL d0 4 + d1
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


rotateCarry_ :: GB D8 -> (D8 -> GB ()) -> (D8 -> Int -> D8) -> Int -> GB ()
rotateCarry_ md p shft test =
  do d <- md
     let res = shft d 1
         zf = res == 0
         cf = testBit d test
     putFlags zf False False cf
     p res

rotate_ :: GB D8 -> (D8 -> GB ()) -> (D8 -> Int -> D8) -> Int -> GB ()
rotate_ md p shft test =
  do d <- md
     cb <- fromBool <$> getCF
     let res = (shft d 1) + cb
         zf = res == 0
         cf = testBit d test
     putFlags zf False False cf
     p res

--

rlca :: GB ()
rlca = rotateCarry_ getA putA shiftL 7

rla :: GB ()
rla = rotate_ getA putA shiftL 7

rrca :: GB ()
rrca = rotateCarry_ getA putA shiftR 0

rra :: GB ()
rra = rotate_ getA putA shiftR 0

rlc_r8 :: R8 -> GB ()
rlc_r8 r = rotateCarry_ (getR8 r) (putR8 r) shiftL 7

rlc_ihl :: GB ()
rlc_ihl = rotateCarry_ getIHL putIHL shiftL 7

rl_r8 :: R8 -> GB ()
rl_r8 r = rotate_ (getR8 r) (putR8 r) shiftL 7

rl_ihl :: R8 -> GB ()
rl_ihl r = rotate_ getIHL putIHL shiftL 7

rrc_r8 :: R8 -> GB ()
rrc_r8 r = rotateCarry_ (getR8 r) (putR8 r) shiftR 0

rrc_ihl :: GB ()
rrc_ihl = rotateCarry_ getIHL putIHL shiftR 0

rr_r8 :: R8 -> GB ()
rr_r8 r = rotate_ (getR8 r) (putR8 r) shiftR 0

rr_ihl :: R8 -> GB ()
rr_ihl r = rotate_ getIHL putIHL shiftR 0

----
