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

dec :: (Num a) => a -> a -> a
dec = subtract

inc :: (Num a) => a -> a -> a
inc = (+)

--

sepR16 :: R16 -> (R8, R8)
sepR16 r = case r of
  AF -> (A, F)
  BC -> (B, C)
  DE -> (D, E)
  SP -> (SP_0, SP_1)
  PC -> (PC_0, PC_1)

sep16 :: D16 -> (D8, D8)
sep16 d =
  let d0 = shiftR (d .&. 0xf0) 4
      d1 = (d .&. 0x0f)
  in (num *** num) (d0, d1)

cmb8 :: D8 -> D8 -> D16
cmb8 a b = (shiftL (num a) 4) .|. (num b)

getR8 :: R8 -> GB D8
getR8 r =
  do rs <- gets regs
     io (readArray rs (fromEnum r))

--

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

--

getR16 :: R16 -> GB D16
getR16 r =
  do rs <- gets regs
     let (r0, r1) = sepR16 r
     d0 <- io (readArray rs (fromEnum r0))
     d1 <- io (readArray rs (fromEnum r1))
     let d0' = num d0
         d1' = num d1
     return (d0' + d1')

--

getAF = getR16 AF
getBC = getR16 BC
getDE = getR16 DE
getHL = getR16 HL
getSP = getR16 SP
getPC = getR16 PC

--

putR8 :: R8 -> D8 -> GB ()
putR8 r d =
  do rs <- gets regs
     io (writeArray rs (fromEnum r) d)

--

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

--

putR16 :: R16 -> D16 -> GB ()
putR16 r d =
  do rs <- gets regs
     let (r0, r1) = sepR16 r
         (d0, d1) = sep16 d
     io (writeArray rs (fromEnum r0) d0)
     io (writeArray rs (fromEnum r1) d1)

--

putAF = putR16 AF
putBC = putR16 BC
putDE = putR16 DE
putHL = putR16 HL
putSP = putR16 SP
putPC = putR16 PC

--

getRam8 :: A16 -> GB D8
getRam8 a =
  do rm <- gets ram
     io (readArray rm a)

getRam16 :: A16 -> GB D16
getRam16 a =
  do rm <- gets ram
     d0 <- io (readArray rm a)
     d1 <- io (readArray rm (a + 1))
     return (cmb8 d0 d1)

--

getIBC = getBC >>= getRam8
getIDE = getDE >>= getRam8
getIHL = getHL >>= getRam8
getISP = getSP >>= getRam16
getIPC = getPC >>= getRam16

--

putRam8 :: A16 -> D8 -> GB ()
putRam8 a d =
  do rm <- gets ram
     io (writeArray rm a d)

putRam16 :: A16 -> D16 -> GB ()
putRam16 a d =
  do rm <- gets ram
     let (d0, d1) = sep16 d
     io (writeArray rm a d0)
     io (writeArray rm (a + 1) d1)

--

putIBC d = getBC >>= (flip putRam8) d
putIDE d = getDE >>= (flip putRam8) d
putIHL d = getHL >>= (flip putRam8) d
putISP d = getSP >>= (flip putRam16) d
putIPC d = getPC >>= (flip putRam16) d

--

flagBit :: Flag -> Int
flagBit flg = case flg of
  ZF -> 7
  NF -> 6
  HF -> 5
  CF -> 4

--

setFlag :: Flag -> Bool -> GB ()
setFlag flg test =
  do d <- getF
     let f = if test then setBit else clearBit
     putF (f d (flagBit flg))

--

setZF = setFlag ZF
setNF = setFlag NF
setHF = setFlag HF
setCF = setFlag CF

--

resetZF = setZF False
resetNF = setNF False
resetHF = setHF False
resetCF = setCF False

--

modifyGB :: (a -> GB b) -> (a -> b -> GB ())
         -> a -> (b -> b)
         -> GB ()
modifyGB g p r f =
  do d <- g r
     p r (f d)

-- 

modifyR8 :: R8 -> (D8 -> D8) -> GB ()
modifyR8 = modifyGB getR8 putR8

modifyR16 :: R16 -> (D16 -> D16) -> GB ()
modifyR16 = modifyGB getR16 putR16

--

incSP d = modifyR16 SP (inc d)
incPC d = modifyR16 PC (inc d)

decSP d = modifyR16 SP (dec d)
decPC d = modifyR16 PC (dec d)

