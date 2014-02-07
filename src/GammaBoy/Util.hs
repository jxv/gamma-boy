module GammaBoy.Util where

import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.IO.Class
import Data.Array.IO
import Data.IORef
import Data.Bits
import Data.Word
import Data.Word.Odd (Word3)
import Data.ByteString
import GammaBoy.Types
import Control.Monad.State.Strict

io :: (MonadIO m) => IO a -> m a
io = liftIO

num :: (Integral a, Num b) => a -> b
num = fromIntegral

--

r16Pair :: R16 -> (R8, R8)
r16Pair r = case r of
  AF -> (A, F)
  BC -> (B, C)
  DE -> (D, E)
  SP -> (SP_0, SP_1)
  PC -> (PC_0, PC_1)

d16Pair :: D16 -> (D8, D8)
d16Pair d =
  let d0 = shiftR (d .&. 0xf0) 4
      d1 = (d .&. 0x0f)
  in (num *** num) (d0, d1)

getR8 :: R8 -> GammaBoy D8
getR8 r =
  do rs <- gets regs
     io (readArray rs (fromEnum r))

getA = getR8 A
getF = getR8 F
getB = getR8 B
getC = getR8 C
getD = getR8 D
getE = getR8 E
getH = getR8 H
getL = getR8 L
getSP_0 = getR8 SP_0
getSP_1 = getR8 SP_1
getPC_0 = getR8 PC_0
getPC_1 = getR8 PC_1

getR16 :: R16 -> GammaBoy D16
getR16 r =
  do rs <- gets regs
     let (r0, r1) = r16Pair r
     d0 <- io (readArray rs (fromEnum r0))
     d1 <- io (readArray rs (fromEnum r1))
     let d0' = num d0
         d1' = num d1
     return (d0' + d1')

getAF = getR16 AF
getBC = getR16 BC
getDE = getR16 DE
getHL = getR16 HL
getSP = getR16 SP
getPC = getR16 PC

putR8 :: R8 -> D8 -> GammaBoy ()
putR8 r d =
  do rs <- gets regs
     io (writeArray rs (fromEnum r) d)

putA = putR8 A
putF = putR8 F
putB = putR8 B
putC = putR8 C
putD = putR8 D
putE = putR8 E
putH = putR8 H
putL = putR8 L
putSP_0 = putR8 SP_0
putSP_1 = putR8 SP_1
putPC_0 = putR8 PC_0
putPC_1 = putR8 PC_1

putR16 :: R16 -> D16 -> GammaBoy ()
putR16 r d =
  do rs <- gets regs
     let (r0, r1) = r16Pair r
         (d0, d1) = d16Pair d
     io (writeArray rs (fromEnum r0) d0)
     io (writeArray rs (fromEnum r1) d1)

putAF = putR16 AF
putBC = putR16 BC
putDE = putR16 DE
putHL = putR16 HL
putSP = putR16 SP
putPC = putR16 PC

getRAM :: A16 -> GammaBoy D8
getRAM a =
  do rm <- gets ram
     io (readArray rm a)

getIBC = getBC >>= getRAM
getIDE = getDE >>= getRAM
getIHL = getHL >>= getRAM

putRAM :: A16 -> D8 -> GammaBoy ()
putRAM a d =
  do rm <- gets ram
     io (writeArray rm a d)

putIBC d = getBC >>= (flip putRAM) d
putIDE d = getDE >>= (flip putRAM) d
putIHL d = getHL >>= (flip putRAM) d

flagBit :: Flag -> Int
flagBit flg = case flg of
  ZF -> 7
  NF -> 6
  HF -> 5
  CF -> 4

setFlag :: Flag -> Bool -> GammaBoy ()
setFlag flg test =
  do d <- getF
     let f = if test then setBit else clearBit
     putF (f d (flagBit flg))

setZF = setFlag ZF
setNF = setFlag NF
setHF = setFlag HF
setCF = setFlag CF

resetZF = setZF False
resetNF = setNF False
resetHF = setHF False
resetCF = setCF False

