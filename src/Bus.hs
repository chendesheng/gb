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
    readLcdCBWindowTileMapArea,
    isLcdCWindowEnable,
    readLcdCBgTileDataArea,
    readLcdCBgTileMapArea,
    readLcdCObjSize,
    readLcdCObjEnable,
    isLcdCBgEnable,
    readLcdC,
    readLcdYC,
    readLYCIntSelect,
    readMode0IntSelect,
    readMode1IntSelect,
    readMode2IntSelect,
    readSCY,
    readSCX,
    readBGPalette,
    readOBP0Palette,
    readOBP1Palette,
    readWY,
    readWX,
    syncPPU,
    OAMEntry(..),
    readOAMEntry,
    TileIndex,
    readTileIndex,
    readBgTileRowLow,
    readBgTileRowHigh,
    Interrupt (..),
    Interrupts,
    readIE,
    writeIE,
    readIF,
    writeIF,
    interruptAddress,
  )
where

import Data.Binary.Get (runGet)
import Data.Bits ((.|.), (.&.), Bits (shiftR, testBit), setBit, clearBit)
import qualified Data.ByteString.Lazy as BL
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
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
    io :: Ram,
    ie :: IORef Word8
  }

initBus :: BL.ByteString -> BL.ByteString -> IO Bus
initBus boot cartridge = do
  vram <- MV.replicate 0x2000 0xCD -- 8000-9FFF
  wram <- MV.replicate 0x2000 0xCD -- C000-DFFF
  oam <- MV.replicate 0x00A0 0xCD -- FE00-FE9F
  hram <- MV.replicate 0x007F 0xCD -- FF80-FFFE
  io <- MV.replicate 0x0080 0x00 -- FF00-FF7F, rough/simple
  ie  <- newIORef 0x00
  return
    Bus
      { boot = byteStringToVector boot,
        cartridge = byteStringToVector cartridge,
        vram,
        wram,
        oam,
        hram,
        io,
        ie
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
  | 0x8000 <= addr && addr < 0xA000 = do
      mode <- readPPUMode bus.io
      if mode == 3 then return 0xFF
      else readRam (addr - 0x8000) bus.vram
  | 0xC000 <= addr && addr < 0xE000 =
      readRam (addr - 0xC000) bus.wram
  | 0xFE00 <= addr && addr < 0xFEA0 = do
      mode <- readPPUMode bus.io
      if mode == 2 || mode == 3 then return 0xFF
      else readRam (addr - 0xFE00) bus.oam
  | 0xFF00 <= addr && addr < 0xFF80 =
      readRam (addr - 0xFF00) bus.io
  | 0xFF80 <= addr && addr < 0xFFFF =
      readRam (addr - 0xFF80) bus.hram
  | addr == 0xFFFF = readIORef bus.ie
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
      -- VRAM is inaccessible in mode 3
      mode <- readPPUMode bus.io
      if mode == 3 then
        return bus
      else do
        writeRam (addr - 0x8000) val bus.vram
        return bus
  | 0xC000 <= addr && addr < 0xE000 = do
      writeRam (addr - 0xC000) val bus.wram
      return bus
  | 0xFE00 <= addr && addr < 0xFEA0 = do
      mode <- readPPUMode bus.io
      if mode == 2 || mode == 3 then
        return bus
      else do
        writeRam (addr - 0xFE00) val bus.oam
        return bus
  | 0xFF41 == addr = do
      -- the lower 3 bits are readonly
      -- FIXME: what about the highest bit?
      b <- readRam 0x41 bus.io
      let b' = b .&. 0x07 -- b00000111
      let val' = val .&. 0xF8 -- b11111000
      writeRam 0x41 (val' .|. b') bus.io
      return bus
  | 0xFF44 == addr = return bus -- LY is readonly
  | 0xFF45 == addr = do -- LY compare
      ly <- readLcdY bus
      status <- readRam 0x41 bus.io
      writeRam 0x45 val bus.io
      if ly == val then do
        writeRam 0x41 (status `setBit` 2) bus.io
      else
        writeRam 0x41 (status `clearBit` 2) bus.io
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
  | addr == 0xFFFF = do
      writeIORef bus.ie val
      return bus
  | otherwise = return bus

readPPUMode :: Ram -> IO Word8
readPPUMode io = do
  status <- readRam 0x41 io
  return $ status .&. 0x03

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

-- https://gbdev.io/pandocs/LCDC.html
readLcdC :: Int -> Bus -> IO Bool
readLcdC index bus = do
  b <- readByte 0xFF40 bus
  return $ (b `shiftR` index .&. 0x01) == 1

isLcdOn :: Bus -> IO Bool
isLcdOn = readLcdC 7

readLcdCBWindowTileMapArea :: Bus -> IO Address
readLcdCBWindowTileMapArea bus = do
  is9C00 <- readLcdC 6 bus
  return $ if is9C00 then 0x9C00 else 0x9800

isLcdCWindowEnable :: Bus -> IO Bool
isLcdCWindowEnable = readLcdC 5

readLcdCBgTileDataArea :: Bus -> IO Address
readLcdCBgTileDataArea bus = do
  is8000 <- readLcdC 4 bus
  return $ if is8000 then 0x8000 else 0x8800

readLcdCBgTileMapArea :: Bus -> IO Address
readLcdCBgTileMapArea bus = do
  is9C00 <- readLcdC 3 bus
  return $ if is9C00 then 0x9C00 else 0x9800

readLcdCObjSize :: Bus -> IO Int
readLcdCObjSize bus = do
  is8x16 <- readLcdC 2 bus
  return $ if is8x16 then 16 else 8

readLcdCObjEnable :: Bus -> IO Bool
readLcdCObjEnable = readLcdC 1

isLcdCBgEnable :: Bus -> IO Bool
isLcdCBgEnable = readLcdC 0

readLcdY :: Bus -> IO Word8
readLcdY = readByte 0xFF44

readLcdYC :: Bus -> IO Word8
readLcdYC = readByte 0xFF45

-- writeLcdYC :: Word8 -> Bus -> IO Bus
-- writeLcdYC = writeByte 0xFF45

readLcdStatus :: Int -> Bus -> IO Bool
readLcdStatus index bus = do
  b <- readRam 0x41 bus.io
  return $ (b `shiftR` index .&. 0x01) == 1

readLYCIntSelect :: Bus -> IO Bool
readLYCIntSelect = readLcdStatus 6

readMode2IntSelect :: Bus -> IO Bool
readMode2IntSelect = readLcdStatus 5

readMode1IntSelect :: Bus -> IO Bool
readMode1IntSelect = readLcdStatus 4

readMode0IntSelect :: Bus -> IO Bool
readMode0IntSelect = readLcdStatus 3

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

readWY :: Bus -> IO Word8
readWY = readByte 0xFF4A

readWX :: Bus -> IO Word8
readWX = readByte 0xFF4B

-- in order make PPU state readable by CPU from Bus
syncPPU :: Word8 -> Word8 -> Bus -> IO ()
syncPPU ly mode bus = do
  -- TODO: there are other things need update
  _ <- writeRam 0x44 ly bus.io
  lyc <- readRam 0x45 bus.io

  status <- readRam 0x41 bus.io
  let status' = if ly == lyc then status `setBit` 2 else status `clearBit` 2
  writeRam 0x41 (status' .&. 0xFC .|. mode) bus.io

-- TODO: OAM DMA transfer
-- https://gbdev.io/pandocs/OAM_DMA_Transfer.html

-- OAM read for PPU
data OAMEntry = OAMEntry
  { yPos :: Int
  , xPos :: Int
  , tileIndex :: Word8
  , attributes :: Word8
  }

readOAMEntry :: Word8 -> Bus -> IO OAMEntry
readOAMEntry i bus = do
  let addr = fromIntegral i * 4
  y <- readRam addr bus.oam
  x <- readRam (addr + 1) bus.oam
  tileIndex <- readRam (addr + 2) bus.oam
  attributes <- readRam (addr + 3) bus.oam
  return $ OAMEntry (fromIntegral y) (fromIntegral x) tileIndex attributes

type TileIndex = Word8

readTileIndex :: Address -> Word8 -> Word8 -> Bus -> IO TileIndex
readTileIndex base y x bus = do
  let y' = fromIntegral y :: Word16
  let x' = fromIntegral x :: Word16
  let addr = base + (y' `div` 8 * 32 + x' `div` 8)
  readRam (addr - 0x8000) bus.vram

readBgTileRowBaseAddress :: TileIndex -> Word8 -> Bus -> IO Address
readBgTileRowBaseAddress index y bus = do
  base <- readLcdCBgTileDataArea bus
  let addr
        | base == 0x8000 = base + tileOffset index + rowOffset
        | index < 128 = 0x9000 + tileOffset index + rowOffset
        | otherwise = 0x8800 + tileOffset (index - 128) + rowOffset
  return addr
  where
    -- each tile taking 16 bytes
    tileOffset i = fromIntegral i * 16
    rowOffset = fromIntegral (y `mod` 8) * 2

readBgTileRowLow :: TileIndex -> Word8 -> Bus -> IO Word8
readBgTileRowLow index y bus = do
  addr <- readBgTileRowBaseAddress index y bus
  readRam (addr - 0x8000) bus.vram

readBgTileRowHigh :: TileIndex -> Word8 -> Bus -> IO Word8
readBgTileRowHigh index y bus = do
  addr <- readBgTileRowBaseAddress index y bus
  readRam (addr + 1 - 0x8000) bus.vram

-- Interruption
data Interrupt = VBlank | LCDStat | Timer | Serial | Joypad  deriving (Show, Eq, Enum)

interruptAddress :: Interrupt -> Address
interruptAddress VBlank = 0x40
interruptAddress LCDStat = 0x48
interruptAddress Timer = 0x50
interruptAddress Serial = 0x58
interruptAddress Joypad = 0x60

filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM _ [] = return []
filterM p (x:xs) = do
  b <- p x
  rest <- filterM p xs
  return (if b then x : rest else rest)

type Interrupts = [Interrupt]

filterInterrupts :: (Bus -> IO Word8) -> Bus -> IO Interrupts
filterInterrupts f bus = filterM (\int -> do
                                    b <- f bus
                                    return $ b `testBit` fromEnum int
                                ) [VBlank .. Joypad]

readIE :: Bus -> IO Interrupts
readIE = filterInterrupts $ readByte 0xFFFF

writeIE :: Interrupt -> Bool -> Bus -> IO ()
writeIE int val bus = do
  b <- readByte 0xFFFF bus
  let bit = fromEnum int
  let update = if val then setBit else clearBit
  _ <- writeByte 0xFFFF (b `update` bit) bus
  return ()

readIF :: Bus -> IO Interrupts
readIF = filterInterrupts $ readByte 0xFF0F

writeIF :: Interrupt -> Bool -> Bus -> IO ()
writeIF int val bus = do
  b <- readByte 0xFF0F bus
  let bit = fromEnum int
  let update = if val then setBit else clearBit
  _ <- writeByte 0xFF0F (b `update` bit) bus
  return ()
