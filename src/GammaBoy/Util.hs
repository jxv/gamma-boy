module GammaBoy.Util where

import GammaBoy.Imports
import GammaBoy.Types

----


io :: (MonadIO m) => IO a -> m a
io = liftIO

num :: (Integral a, Num b) => a -> b
num = fromIntegral

shiftR' :: U8 -> Int -> U8
shiftR' d s = (shiftR d s) .|. (d .&. (complement (shiftR d 0xff)))

-----


r8 :: R8 -> Int
r8 = fromEnum

sepR16 :: R16 -> (R8, R8)
sepR16 r = case r of
  AF -> (A, F)
  BC -> (B, C)
  DE -> (D, E)
  HL -> (H, L)
  SP -> (SP_0, SP_1)
  PC -> (PC_0, PC_1)

sep16 :: U16 -> (U8, U8)
sep16 d =
  let d0 = shiftR (d .&. 0xf0) 4
      d1 = (d .&. 0x0f)
  in (num *** num) (d0, d1)

cmb8 :: U8 -> U8 -> U16
cmb8 a b = (shiftL (num a) 4) .|. (num b)

----


getR8 :: R8 -> GB U8
getR8 r =
  do rs <- gets regs
     io (readArray rs (r8 r))

getR16 :: R16 -> GB U16
getR16 r =
  do rs <- gets regs
     let (r0, r1) = sepR16 r
     d0 <- io (readArray rs (r8 r0))
     d1 <- io (readArray rs (r8 r1))
     let d0' = num d0
         d1' = num d1
     return (d0' + d1')

putR8 :: R8 -> U8 -> GB ()
putR8 r d =
  do rs <- gets regs
     io (writeArray rs (r8 r) d)

putR16 :: R16 -> U16 -> GB ()
putR16 r d =
  do rs <- gets regs
     let (r0, r1) = sepR16 r
         (d0, d1) = sep16 d
     io (writeArray rs (r8 r0) d0)
     io (writeArray rs (r8 r1) d1)

--

getA :: GB U8
getA = getR8 A

getF :: GB U8
getF = getR8 F

getB :: GB U8
getB = getR8 B

getC :: GB U8
getC = getR8 C

getD :: GB U8
getD = getR8 D

getE :: GB U8
getE = getR8 E

getH :: GB U8
getH = getR8 H

getL :: GB U8
getL = getR8 L

getSP_0 :: GB U8
getSP_0 = getR8 SP_0

getSP_1 :: GB U8
getSP_1 = getR8 SP_1

getPC_0 :: GB U8
getPC_0 = getR8 PC_0

getPC_1 :: GB U8
getPC_1 = getR8 PC_1

getAF :: GB U16
getAF = getR16 AF

getBC :: GB U16
getBC = getR16 BC

getDE :: GB U16
getDE = getR16 DE

getHL :: GB U16
getHL = getR16 HL

getSP :: GB U16
getSP = getR16 SP

getPC :: GB U16
getPC = getR16 PC

putA :: U8 -> GB ()
putA = putR8 A

putF :: U8 -> GB ()
putF = putR8 F

putB :: U8 -> GB ()
putB = putR8 B

putC :: U8 -> GB ()
putC = putR8 C

putD :: U8 -> GB ()
putD = putR8 D

putE :: U8 -> GB ()
putE = putR8 E

putH :: U8 -> GB ()
putH = putR8 H

putL :: U8 -> GB ()
putL = putR8 L

putSP_0 :: U8 -> GB ()
putSP_0 = putR8 SP_0

putSP_1 :: U8 -> GB ()
putSP_1 = putR8 SP_1

putPC_0 :: U8 -> GB ()
putPC_0 = putR8 PC_0

putPC_1 :: U8 -> GB ()
putPC_1 = putR8 PC_1

putAF :: U16 -> GB ()
putAF = putR16 AF

putBC :: U16 -> GB ()
putBC = putR16 BC

putDE :: U16 -> GB ()
putDE = putR16 DE

putHL :: U16 -> GB ()
putHL = putR16 HL

putSP :: U16 -> GB ()
putSP = putR16 SP

putPC :: U16 -> GB ()
putPC = putR16 PC

----


getRam8 :: A16 -> GB U8
getRam8 a =
  do rm <- gets ram
     io (readArray rm a)

getRam16 :: A16 -> GB U16
getRam16 a =
  do rm <- gets ram
     d0 <- io (readArray rm a)
     d1 <- io (readArray rm (a + 1))
     return (cmb8 d0 d1)

putRam8 :: A16 -> U8 -> GB ()
putRam8 a d =
  do rm <- gets ram
     io (writeArray rm a d)

putRam16 :: A16 -> U16 -> GB ()
putRam16 a d =
  do rm <- gets ram
     let (d0, d1) = sep16 d
     io (writeArray rm a d0)
     io (writeArray rm (a + 1) d1)

--

getIBC :: GB U8
getIBC = getBC >>= getRam8

getIDE :: GB U8
getIDE = getDE >>= getRam8

getIHL :: GB U8
getIHL = getHL >>= getRam8

getISP :: GB U16
getISP = getSP >>= getRam16

getIPC :: GB U16
getIPC = getPC >>= getRam16

putIBC :: U8 -> GB ()
putIBC d = getBC >>= (flip putRam8) d

putIDE :: U8 -> GB ()
putIDE d = getDE >>= (flip putRam8) d

putIHL :: U8 -> GB ()
putIHL d = getHL >>= (flip putRam8) d

putISP :: U16 -> GB ()
putISP d = getSP >>= (flip putRam16) d

putIPC :: U16 -> GB ()
putIPC d = getPC >>= (flip putRam16) d

----


flagBit :: Flag -> Int
flagBit flg = case flg of
  ZF -> 7
  NF -> 6
  HF -> 5
  CF -> 4

--


getFlag :: Flag -> GB Bool
getFlag flg =
  do flgs <- getF
     return (testBit flgs (flagBit flg))

getFlags :: GB (Bool, Bool, Bool, Bool)
getFlags =
  do flgs <- getF
     let f flg = testBit flgs (flagBit flg)
     return (f ZF, f NF, f HF, f CF)

putFlags :: Bool -> Bool -> Bool -> Bool -> GB ()
putFlags zf nf cf hf =
  do let f t flg = if t then bit (flagBit flg) else 0
         zb = f zf ZF 
         nb = f nf NF 
         hb = f hf HF 
         cb = f cf CF
         flgs = zb .|. nb .|. hb .|. cb
     putF flgs


--

getZF :: GB Bool
getZF = getFlag ZF

getNF :: GB Bool
getNF = getFlag NF

getHF :: GB Bool
getHF = getFlag HF

getCF :: GB Bool
getCF = getFlag CF

----


modifyR16 :: R16 -> (U16 -> U16) -> GB ()
modifyR16 r f =
  do d <- getR16 r
     putR16 r (f d)

--

incSP :: U16 -> GB ()
incSP d = modifyR16 SP ((+) d)

incPC :: U16 -> GB ()
incPC d = modifyR16 PC ((+) d)

decSP :: U16 -> GB ()
decSP d = modifyR16 SP ((-) d)

decPC :: U16 -> GB ()
decPC d = modifyR16 PC ((-) d)

