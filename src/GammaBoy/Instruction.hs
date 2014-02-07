module GammaBoy.Instruction where

import Control.Monad.State.Strict
import Data.Array.IO
import Data.IORef
import Data.Word
import Data.Word.Odd (Word3)
import Data.ByteString
import Data.Bits
import GammaBoy.Types
import GammaBoy.Util

cycles :: (Integral a) => Instruction -> a
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
  LDHL_sp_d8  -> 12
  LD_a16_sp   -> 20
  PUSH_s16    -> 16
  POP_s16     -> 12
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

cyclesFromCCSuccess :: (Integral a) => Instruction -> a
cyclesFromCCSuccess instr = case instr of
  JP_cc_a16   -> 4
  JR_cc_a8    -> 4
  CALL_cc_a16 -> 12
  RET_cc      -> 12
  _ -> 0

--

putAFromRAM :: A16 -> GammaBoy ()
putAFromRAM a =
  do d <- getRAM a
     putA d

putRAMFromA :: A16 -> GammaBoy ()
putRAMFromA a =
  do d <- getA
     putRAM a d

--

putAFromRAMOffset :: A8 -> GammaBoy ()
putAFromRAMOffset a = putAFromRAM (0xff00 + num a)

putRAMOffsetFromA :: A8 -> GammaBoy ()
putRAMOffsetFromA a = putRAMFromA (0xff00 + num a)

--

ld_r8_r8 :: R8 -> R8 -> GammaBoy ()
ld_r8_r8 r0 r1 =
  do d1 <- getR8 r1
     putR8 r0 d1

ld_r8_ihl :: R8 -> GammaBoy ()
ld_r8_ihl r =
  do d <- getIHL
     putR8 r d

ld_ihl_r8 :: R8 -> GammaBoy ()
ld_ihl_r8 r =
  do d <- getR8 r
     putIHL d

ld_ihl_d8 :: D8 -> GammaBoy ()
ld_ihl_d8 d = putIHL d

ld_a_idr :: R16 -> GammaBoy ()
ld_a_idr r =
  do a <- getR16 r
     putAFromRAM a

ld_a_a16 :: A16 -> GammaBoy ()
ld_a_a16 = putAFromRAM

ld_a_d8 :: D8 -> GammaBoy ()
ld_a_d8 = putA

ld_idr_a :: R16 -> GammaBoy ()
ld_idr_a r =
  do a <- getR16 r
     putRAMFromA a

ld_a16_a :: A16 -> GammaBoy ()
ld_a16_a = putRAMFromA

ld_a_idr_c :: GammaBoy ()
ld_a_idr_c =
  do a <- getC
     putAFromRAMOffset a

ld_idr_c_a :: GammaBoy ()
ld_idr_c_a =
  do a <- getC
     putRAMOffsetFromA a

ldh_a8_a :: A8 -> GammaBoy ()
ldh_a8_a a = putRAMOffsetFromA a

ld_r16_d16 :: R16 -> D16 -> GammaBoy ()
ld_r16_d16 = putR16

ld_sp_hl :: GammaBoy ()
ld_sp_hl =
  do d <- getHL
     putSP d

ldhl_sp_d8 :: D8 -> GammaBoy ()
ldhl_sp_d8 d =
  do let k = num d
     v <- getSP
     let w = v + k
     resetZF
     resetNF
     setCF (w < v || w < k)
     setHF (testBit ((v .&. 0x0f) + (k .&. 0x0f)) 4)
     putHL w

ld_a16_sp :: A16 -> GammaBoy ()
ld_a16_sp a =
  do d0 <- getSP_0
     d1 <- getSP_1
     putRAM a       d0
     putRAM (a + 1) d1

