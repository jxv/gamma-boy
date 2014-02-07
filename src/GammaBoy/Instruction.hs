module GammaBoy.Inst where

import Control.Monad.State.Strict
import Control.Applicative
import Data.Array.IO
import Data.IORef
import Data.Word
import Data.Word.Odd (Word3)
import Data.ByteString
import Data.Bits
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
  ADD_sp_d8   -> 16
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

putAFromRam8 :: A16 -> GB
putAFromRam8 a =
  do d <- getRam8 a
     putA d

putRam8FromA :: A16 -> GB
putRam8FromA a =
  do d <- getA
     putRam8 a d

--

putAFromRam8Offset :: A8 -> GB
putAFromRam8Offset a = putAFromRam8 (0xff00 + num a)

putRam8OffsetFromA :: A8 -> GB
putRam8OffsetFromA a = putRam8FromA (0xff00 + num a)

--

ld_r8_r8 :: R8 -> R8 -> GB
ld_r8_r8 r0 r1 =
  do d1 <- getR8 r1
     putR8 r0 d1

ld_r8_ihl :: R8 -> GB
ld_r8_ihl r =
  do d <- getIHL
     putR8 r d

ld_ihl_r8 :: R8 -> GB
ld_ihl_r8 r =
  do d <- getR8 r
     putIHL d

ld_ihl_d8 :: D8 -> GB
ld_ihl_d8 d = putIHL d

ld_a_idr :: R16 -> GB
ld_a_idr r =
  do a <- getR16 r
     putAFromRam8 a

ld_a_a16 :: A16 -> GB
ld_a_a16 = putAFromRam8

ld_a_d8 :: D8 -> GB
ld_a_d8 = putA

ld_idr_a :: R16 -> GB
ld_idr_a r =
  do a <- getR16 r
     putRam8FromA a

ld_a16_a :: A16 -> GB
ld_a16_a = putRam8FromA

ld_a_idr_c :: GB
ld_a_idr_c =
  do a <- getC
     putAFromRam8Offset a

ld_idr_c_a :: GB
ld_idr_c_a =
  do a <- getC
     putRam8OffsetFromA a

ldh_a8_a :: A8 -> GB
ldh_a8_a a = putRam8OffsetFromA a

ld_r16_d16 :: R16 -> D16 -> GB
ld_r16_d16 = putR16

ld_sp_hl :: GB
ld_sp_hl =
  do d <- getHL
     putSP d

ldhl_sp_s8 :: D8 -> GB
ldhl_sp_s8 s =
  do sp <- getSP
     let d = num (0x7f .&. s)
     if isSigned s -- is negative?
        then do let res = sp - d
                    hf = (sp .&. 0x000f) < d
                    cf = res > sp
                setFlags False False hf cf
                putHL res 
        else do let res = sp + d
                    hf = testBit ((sp .&. 0x000f) + d) 4
                    cf = res < sp || res < d
                setFlags False False hf cf
                putHL res 

ld_a16_sp :: A16 -> GB
ld_a16_sp a =
  do sp <- getSP
     putRam16 a sp

--

push_r16 :: R16 -> GB
push_r16 r =
  do d <- getR16 r
     putISP d
     decSP 2

pop_r16 :: R16 -> GB
pop_r16 r =
  do d <- getISP
     putR16 r d
     incSP 2

----

addA_ :: D8 -> GB
addA_ d =
  do k <- getA
     let low  = (d .&. 0x0f) + (k .&. 0x0f)
         res  = d + k
         zf = res == 0
         cf = res < d || res < k
         hf = testBit low 4
     setFlags zf False cf hf
     putA res

add_a_r8 :: R8 -> GB
add_a_r8 r =
  do d <- getR8 r
     addA_ d

add_a_ihl :: GB
add_a_ihl =
  do d <- getIHL
     addA_ d
     
add_a_d8 :: D8 -> GB
add_a_d8 = addA_

--


