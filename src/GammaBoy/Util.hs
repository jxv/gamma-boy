module GammaBoy.Util where

import GammaBoy.Imports
import GammaBoy.Types

----

io :: (MonadIO m) => IO a -> m a
io = liftIO

num :: (Integral a, Num b) => a -> b
num = fromIntegral

shiftR' :: U8 -> Int -> U8
shiftR' u s = (shiftR u s) .|. (u .&. (complement (shiftR u 0xff)))

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
sep16 u =
  let u0 = shiftR (u .&. 0xf0) 4
      u1 = (u .&. 0x0f)
  in (num *** num) (u0, u1)

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
     u0 <- io (readArray rs (r8 r0))
     u1 <- io (readArray rs (r8 r1))
     let u0' = num u0
         u1' = num u1
     return (u0' + u1')

putR8 :: R8 -> U8 -> GB ()
putR8 r u =
  do rs <- gets regs
     io (writeArray rs (r8 r) u)

putR16 :: R16 -> U16 -> GB ()
putR16 r u =
  do rs <- gets regs
     let (r0, r1) = sepR16 r
         (u0, u1) = sep16 u
     io (writeArray rs (r8 r0) u0)
     io (writeArray rs (r8 r1) u1)

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
     u0 <- io (readArray rm a)
     u1 <- io (readArray rm (a + 1))
     return (cmb8 u0 u1)

putRam8 :: A16 -> U8 -> GB ()
putRam8 a u =
  do rm <- gets ram
     io (writeArray rm a u)

putRam16 :: A16 -> U16 -> GB ()
putRam16 a u =
  do rm <- gets ram
     let (u0, u1) = sep16 u
     io (writeArray rm a u0)
     io (writeArray rm (a + 1) u1)

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
putIBC u = getBC >>= (flip putRam8) u

putIDE :: U8 -> GB ()
putIDE u = getDE >>= (flip putRam8) u

putIHL :: U8 -> GB ()
putIHL u = getHL >>= (flip putRam8) u

putISP :: U16 -> GB ()
putISP u = getSP >>= (flip putRam16) u

putIPC :: U16 -> GB ()
putIPC u = getPC >>= (flip putRam16) u


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
  do u <- getR16 r
     putR16 r (f u)

----

incHL :: U16 -> GB ()
incHL u = modifyR16 HL ((+) u)

incSP :: U16 -> GB ()
incSP u = modifyR16 SP ((+) u)

incPC :: U16 -> GB ()
incPC u = modifyR16 PC ((+) u)

decHL :: U16 -> GB ()
decHL u = modifyR16 HL ((-) u)

decSP :: U16 -> GB ()
decSP u = modifyR16 SP ((-) u)

decPC :: U16 -> GB ()
decPC u = modifyR16 PC ((-) u)

----

getIDR :: IDR -> GB U16
getIDR r = case r of
  IBC  -> getR16 BC
  IDE  -> getR16 DE
  IHLP ->
    do a <- getHL
       u <- getRam16 a
       putHL (a + 1)
       return u
  IHLN ->
    do a <- getHL
       u <- getRam16 a
       putHL (a - 1)
       return u

----

getCycles :: GB S8
getCycles =
  do rc <- gets cycles
     io (readIORef rc)

putCycles :: S8 -> GB ()
putCycles s = 
  do rc <- gets cycles
     io (writeIORef rc s)

