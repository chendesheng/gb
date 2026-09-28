module Bus
  ( Address,
    Bus (..),
    fetchInstruction,
    initBus,
    readByte,
    readByteHighMemory,
    readR16,
    readR8,
    writeByte,
    writeByteHighMemory,
    writeR16,
    writeR8,
    readR16Mem,
    bootRomEnabled,
    isLcdOn,
    readLcdC,
    readLcdY,
    readLcdYC,
    readLcdStatus,
    readSCY,
    readSCX,
    readBGPalette,
    readOBP0Palette,
    readOBP1Palette,
  )
where

import Data.Binary.Get (runGet)
import Data.Bits ((.|.), (.&.), Bits (shiftR))
import qualified Data.ByteString.Lazy as BL
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.Word
import Instruction (Instruction, instructionDecoder)
import Registers
import Prelude hiding (length)
import Color

type Rom = Vector Word8

readRom :: Address -> Rom -> Word8
readRom addr rom = rom ! fromIntegral addr

type Ram = MV.IOVector Word8

readRam :: Address -> Ram -> IO Word8
readRam addr ram = MV.read ram $ fromIntegral addr

writeRam :: Address -> Word8 -> Ram -> IO ()
writeRam addr val ram = MV.write ram (fromIntegral addr) val

type Address = Word16

data Bus = Bus
  { boot :: Rom,
    cartridge :: Rom,
    vram :: Ram,
    wram :: Ram,
    oam :: Ram,
    hram :: Ram,
    io :: Ram
  }

initBus :: BL.ByteString -> BL.ByteString -> IO Bus
initBus boot cartridge = do
  vram <- MV.replicate 0x2000 0xCD -- 8000-9FFF
  wram <- MV.replicate 0x2000 0xCD -- C000-DFFF
  oam <- MV.replicate 0x00A0 0xCD -- FE00-FE9F
  hram <- MV.replicate 0x007F 0xCD -- FF80-FFFE
  io <- MV.replicate 0x0080 0x00 -- FF00-FF7F, rough/simple
  return
    Bus
      { boot = byteStringToVector boot,
        cartridge = byteStringToVector cartridge,
        vram,
        wram,
        oam,
        hram,
        io
      }

readByte0xFF50 :: Bus -> IO Word8
readByte0xFF50 bus =  MV.read bus.io 0x50

bootRomEnabled :: Bus -> IO Bool
bootRomEnabled bus = do
  b <- readByte0xFF50 bus
  return $ b == 0
 
readByte :: Address -> Bus -> IO Word8
readByte addr bus
  | addr < 0x0100 = do
      enabled <- bootRomEnabled bus
      return $ readRom addr (if enabled then bus.boot else bus.cartridge)
  | addr < 0x8000 =
      return $ readRom addr bus.cartridge
  | 0x8000 <= addr && addr < 0xA000 =
      readRam (addr - 0x8000) bus.vram
  | 0xC000 <= addr && addr < 0xE000 =
      readRam (addr - 0xC000) bus.wram
  | 0xFE00 <= addr && addr < 0xFEA0 =
      readRam (addr - 0xFE00) bus.oam
  | 0xFF00 <= addr && addr < 0xFF80 =
      readRam (addr - 0xFF00) bus.io
  | 0xFF80 <= addr && addr < 0xFFFF =
      readRam (addr - 0xFF80) bus.hram
  | addr == 0xFFFF = return 0 -- TODO
  | otherwise = return 0xFF

readBytes :: Address -> Int -> Bus -> IO [Word8]
readBytes addr n bus
  | n <= 0 = return []
  | otherwise = do
      b <- readByte addr bus
      bs <- readBytes (addr + 1) (n - 1) bus
      return $ b : bs

writeByte :: Address -> Word8 -> Bus -> IO Bus
writeByte addr val bus
  -- addr < 0x8000  Usually cartridge/MBC control
  | 0x8000 <= addr && addr < 0xA000 = do
      writeRam (addr - 0x8000) val bus.vram
      return bus
  | 0xC000 <= addr && addr < 0xE000 = do
      writeRam (addr - 0xC000) val bus.wram
      return bus
  | 0xFE00 <= addr && addr < 0xFEA0 = do
      writeRam (addr - 0xFE00) val bus.oam
      return bus
  | 0xFF50 == addr = do
      -- 0xFF50 disables boot ROM
      b <- readByte0xFF50 bus
      writeRam 0x50 (val .|. b) bus.io
      return bus
  | 0xFF00 <= addr && addr < 0xFF80 = do
      writeRam (addr - 0xFF00) val bus.io
      return bus
  | 0xFF80 <= addr && addr < 0xFFFF = do
      writeRam (addr - 0xFF80) val bus.hram
      return bus
  | addr == 0xFFFF = return bus
  | otherwise = return bus

readByteHighMemory :: Word8 -> Bus -> IO Word8
readByteHighMemory offset = readByte (0xFF00 + fromIntegral offset)

writeByteHighMemory :: Word8 -> Word8 -> Bus -> IO Bus
writeByteHighMemory offset = writeByte $ 0xFF00 + fromIntegral offset

byteStringToVector :: BL.ByteString -> V.Vector Word8
byteStringToVector bs =
  V.generate (fromIntegral (BL.length bs)) (BL.index bs . fromIntegral)

fetchInstruction :: Address -> Bus -> IO Instruction
fetchInstruction addr bus = do
  bs <- readBytes addr 3 bus
  return $ runGet instructionDecoder (BL.pack bs)

readR8 :: Registers -> Bus -> R8 -> IO Word8
readR8 Registers {rB} _ B = return rB
readR8 Registers {rC} _ C = return rC
readR8 Registers {rD} _ D = return rD
readR8 Registers {rE} _ E = return rE
readR8 Registers {rH} _ H = return rH
readR8 Registers {rL} _ L = return rL
readR8 regs bus AtHL = readByte (getHL regs) bus
readR8 Registers {rA} _ A = return rA

writeR8 :: Registers -> Bus -> R8 -> Word8 -> IO Registers
writeR8 regs _ B val = return $ regs {rB = val}
writeR8 regs _ C val = return $ regs {rC = val}
writeR8 regs _ D val = return $ regs {rD = val}
writeR8 regs _ E val = return $ regs {rE = val}
writeR8 regs _ H val = return $ regs {rH = val}
writeR8 regs _ L val = return $ regs {rL = val}
writeR8 regs bus AtHL val = do
  _ <- writeByte (getHL regs) val bus
  return regs
writeR8 regs _ A val = return $ regs {rA = val}

readR16Mem :: Registers -> R16Mem -> Word16
readR16Mem regs BCm = getBC regs
readR16Mem regs DEm = getDE regs
readR16Mem regs HLi = getHL regs
readR16Mem regs HLd = getHL regs

readR16 :: Registers -> R16 -> Word16
readR16 regs BC = getBC regs
readR16 regs DE = getDE regs
readR16 regs HL = getHL regs
readR16 regs SP = regs.rSP

writeR16 :: Registers -> R16 -> Word16 -> Registers
writeR16 regs r16 val =
  let (h, l) = toWord8s val
   in case r16 of
        BC -> regs {rB = h, rC = l}
        DE -> regs {rD = h, rE = l}
        HL -> regs {rH = h, rL = l}
        SP -> regs {rSP = val}

readLcdC :: Int -> Bus -> IO Bool
readLcdC index bus = do
  b <- readByte 0xFF40 bus
  return $ (b `shiftR` index .&. 0x01) == 1

isLcdOn :: Bus -> IO Bool
isLcdOn = readLcdC 7

readLcdY :: Bus -> IO Word8
readLcdY = readByte 0xFF44

readLcdYC :: Bus -> IO Word8
readLcdYC = readByte 0xFF45

-- writeLcdYC :: Word8 -> Bus -> IO Bus
-- writeLcdYC = writeByte 0xFF45

readLcdStatus :: Int -> Bus -> IO Bool
readLcdStatus index bus = do
  b <- readByte 0xFF41 bus
  return $ (b `shiftR` index .&. 0x01) == 1

readSCY :: Bus -> IO Word8
readSCY = readByte 0xFF42

readSCX :: Bus -> IO Word8
readSCX = readByte 0xFF43

readBGPalette :: Bus -> IO ColorPalette
readBGPalette = readByte 0xFF47

readOBP0Palette :: Bus -> IO ColorPalette
readOBP0Palette = readByte 0xFF48

readOBP1Palette :: Bus -> IO ColorPalette
readOBP1Palette = readByte 0xFF49


