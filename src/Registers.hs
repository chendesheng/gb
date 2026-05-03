module Registers where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.Word
import Dbg

data Cond = NZ | Z | NC | Cc
  deriving (Show, Eq)

data R8 = B | C | D | E | H | L | AtHL | A
  deriving (Show, Eq, Enum)

data R16 = BC | DE | HL | SP
  deriving (Show, Eq)

data R16Stk = BCstk | DEstk | HLstk | AFstk
  deriving (Show, Eq)

data R16Mem = BCm | DEm | HLi | HLd
  deriving (Show, Eq)

data Registers = Registers
  { rA :: Word8,
    rF :: Word8, -- ZNHC0000
    rB :: Word8,
    rC :: Word8,
    rD :: Word8,
    rE :: Word8,
    rH :: Word8,
    rL :: Word8,
    rSP :: Word16,
    rPC :: Word16
  }
  deriving (Show, Eq)

zflag :: Registers -> Bool
zflag regs = (regs.rF .&. 0x80) /= 0

setZflag :: Bool -> Registers -> Registers
setZflag z regs = regs {rF = regs.rF .&. 0x7F .|. (if z then 0x80 else 0)}

nflag :: Registers -> Bool
nflag regs = (regs.rF .&. 0x40) /= 0

setNflag :: Bool -> Registers -> Registers
setNflag n regs = regs {rF = regs.rF .&. 0xBF .|. (if n then 0x40 else 0)}

hflag :: Registers -> Bool
hflag regs = (regs.rF .&. 0x20) /= 0

setHflag :: Bool -> Registers -> Registers
setHflag h regs = regs {rF = regs.rF .&. 0xDF .|. (if h then 0x20 else 0)}

cflag :: Registers -> Bool
cflag regs = (regs.rF .&. 0x10) /= 0

setCflag :: Bool -> Registers -> Registers
setCflag c regs = regs {rF = regs.rF .&. 0xEF .|. (if c then 0x10 else 0)}

fromWord8s :: Word8 -> Word8 -> Word16
fromWord8s h l = fromIntegral h `shiftL` 8 .|. fromIntegral l

getAF :: Registers -> Word16
getAF (Registers {rA, rF}) = fromWord8s rA rF

setAF :: Word16 -> Registers -> Registers
setAF val regs =
  let (h, l) = toWord8s val
   in regs {rA = h, rF = l}

getBC :: Registers -> Word16
getBC (Registers {rB, rC}) = fromWord8s rB rC

setBC :: Word16 -> Registers -> Registers
setBC val regs =
  let (h, l) = toWord8s val
   in regs {rB = h, rC = l}

getDE :: Registers -> Word16
getDE (Registers {rD, rE}) = fromWord8s rD rE

setDE :: Word16 -> Registers -> Registers
setDE val regs =
  let (h, l) = toWord8s val
   in regs {rD = h, rE = l}

getHL :: Registers -> Word16
getHL (Registers {rH, rL}) = fromWord8s rH rL

setHL :: Word16 -> Registers -> Registers
setHL val regs =
  let (h, l) = toWord8s val
   in regs {rH = h, rL = l}

setR16Stk :: R16Stk -> Word16 -> Registers -> Registers
setR16Stk AFstk = setAF
setR16Stk BCstk = setBC
setR16Stk DEstk = setDE
setR16Stk HLstk = setHL

getR16Stk :: R16Stk -> Registers -> Word16
getR16Stk AFstk = getAF
getR16Stk BCstk = getBC
getR16Stk DEstk = getDE
getR16Stk HLstk = getHL

initialRegisters :: Registers
initialRegisters =
  Registers
    { rA = 0xCD,
      rF = 0xC0,
      rB = 0xCD,
      rC = 0xCD,
      rD = 0xCD,
      rE = 0xCD,
      rH = 0xCD,
      rL = 0xCD,
      rSP = 0xCD,
      rPC = 0x00
    }

toWord8s :: Word16 -> (Word8, Word8)
toWord8s w16 = (fromIntegral $ w16 `shiftR` 8, fromIntegral $ w16 .&. 0x00FF)

incHL :: Registers -> Registers
incHL regs =
  let (h, l) = toWord8s $ getHL regs + 1
   in regs {rH = h, rL = l}

decHL :: Registers -> Registers
decHL regs =
  let (h, l) = toWord8s $ getHL regs - 1
   in regs {rH = h, rL = l}

updateR16MemHL :: R16Mem -> Registers -> Registers
updateR16MemHL HLi = incHL
updateR16MemHL HLd = decHL
updateR16MemHL _ = id

getB3 :: Word8 -> Word8 -> Bool
getB3 b3 val = val `shiftR` fromIntegral b3 .&. 0x01 /= 0
