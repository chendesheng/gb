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
    readLcdCWindowTileMapArea,
    isLcdCWindowEnable,
    readLcdCBgTileDataArea,
    readLcdCBgTileMapArea,
    readLcdCObjSize,
    isLcdCObjEnable,
    isLcdCBgEnable,
    readLcdC,
    readLcdYC,
    readLYCIntSelect,
    readMode0IntSelect,
    readMode1IntSelect,
    readMode2IntSelect,
    readSCY,
    readSCX,
    readSCXInt,
    readBGPalette,
    readOBP0Palette,
    readOBP1Palette,
    readWY,
    readWX,
    readWXInt,
    readVRam,
    syncPPU,
    OAMObjectPosition(..),
    OAMObjectAttributes(..),
    readOAMPosition,
    readOAMAttributes,
    TileIndex,
    readBgTileRowBaseAddress,
    Interrupt (..),
    Interrupts,
    readIE,
    writeIE,
    readIF,
    writeIF,
    interruptAddress,
    readObjPalette,
    setJoypad,
    JoypadKey(..),
    resetDIV,
    increaseTimer,
  )
where

import Data.Binary.Get (runGet)
import Data.Bits ((.|.), (.&.), (.>>.), testBit, setBit, clearBit, complement)
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
import Control.Monad (unless, when)

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
    io :: Ram,
    hram :: Ram,
    ie :: IORef Word8,
    joypad :: IORef Word8,
    systemCounter :: IORef Word16,
    timaOverflow :: IORef TIMAOverflow
  }

data TIMAOverflow = NoTIMAOverflow | TIMAOverflowDelay | TIMAOverflowReloading

initBus :: BL.ByteString -> BL.ByteString -> IO Bus
initBus boot cartridge = do
  vram <- MV.replicate 0x2000 0xCD -- 8000-9FFF
  wram <- MV.replicate 0x2000 0xCD -- C000-DFFF
  oam <- MV.replicate 0x00A0 0xCD -- FE00-FE9F
  io <- MV.replicate 0x0080 0x00 -- FF00-FF7F, rough/simple
  writeRam 0 0x0F io
  hram <- MV.replicate 0x007F 0xCD -- FF80-FFFE
  ie  <- newIORef 0x00
  joypad <- newIORef 0xFF
  systemCounter <- newIORef 0
  timaOverflow <- newIORef NoTIMAOverflow
  return
    Bus
      { boot = byteStringToVector boot,
        cartridge = byteStringToVector cartridge,
        vram,
        wram,
        oam,
        hram,
        io,
        ie,
        joypad,
        systemCounter,
        timaOverflow
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
  | 0xFF00 == addr = do
      selection <- readRam 0 bus.io
      directions <- if selection `testBit` 4 then
                      return 0xF
                    else do
                      joypad <- readIORef bus.joypad
                      return $ (joypad .&. 0xF0) .>>. 4
      buttons <- if selection `testBit` 5 then
                    return 0xF
                  else do
                    joypad <- readIORef bus.joypad
                    return $ joypad .&. 0x0F
      return $ 0xC0 .|. (0x30 .&. selection) .|. (directions .&. buttons)
  | 0xFF04 == addr = do
    counter <- readIORef bus.systemCounter
    return $ fromIntegral $ counter .>>. 6
  | 0xFF07 == addr = do
    val <- readRam 7 bus.io
    return $ val .|. 0xF8
  | 0xFF00 < addr && addr < 0xFF80 =
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

writeByte :: Address -> Word8 -> Bus -> IO ()
writeByte addr val bus
  -- addr < 0x8000  Usually cartridge/MBC control
  | 0x8000 <= addr && addr < 0xA000 = do
      -- VRAM is inaccessible in mode 3
      mode <- readPPUMode bus.io
      if mode == 3 then
        return ()
      else do
        writeRam (addr - 0x8000) val bus.vram
  | 0xC000 <= addr && addr < 0xE000 = do
      writeRam (addr - 0xC000) val bus.wram
  | 0xFE00 <= addr && addr < 0xFEA0 = do
      mode <- readPPUMode bus.io
      if mode == 2 || mode == 3 then
        return ()
      else do
        writeRam (addr - 0xFE00) val bus.oam
  | 0xFF00 == addr = do
    oldJoyp <- readJOYP bus
    writeRam 0 val bus.io
    joyp <- readJOYP bus
    requestJoypadInt oldJoyp joyp bus
  | 0xFF04 == addr = resetDIV bus
  | 0xFF05 == addr = do
    overflow <- readIORef bus.timaOverflow
    case overflow of
      NoTIMAOverflow ->
        writeRam 5 val bus.io
      TIMAOverflowDelay -> do
        writeRam 5 val bus.io
        clearTIMAOverflow bus
      TIMAOverflowReloading ->
        return ()
  | 0xFF06 == addr = do
    writeRam 6 val bus.io
    overflow <- readIORef bus.timaOverflow
    case overflow of
      TIMAOverflowReloading -> writeRam 5 val bus.io
      _ -> return ()
  | 0xFF07 == addr = do
    detectFallingEdge (\bus' -> writeRam 7 val bus'.io) bus
  | 0xFF40 == addr = do
      updateSTATInterrupt bus $ do
        wasOn <- isLcdOn bus
        writeRam 0x40 val bus.io
        if not (val `testBit` 7) then
          syncPPURegisters 0 0 bus
        else
          unless wasOn $ syncPPURegisters 0 2 bus
  | 0xFF41 == addr = do
      updateSTATInterrupt bus $ do
        -- the lower 3 bits are readonly
        -- FIXME: what about the highest bit?
        b <- readRam 0x41 bus.io
        let b' = b .&. 0x07 -- b00000111
        let val' = val .&. 0xF8 -- b11111000
        writeRam 0x41 (val' .|. b') bus.io
      return ()
  | 0xFF44 == addr = return () -- LY is readonly
  | 0xFF45 == addr = do -- LY compare
      updateSTATInterrupt bus $ do
        ly <- readLcdY bus
        status <- readRam 0x41 bus.io
        writeRam 0x45 val bus.io
        if ly == val then do
          writeRam 0x41 (status `setBit` 2) bus.io
        else
          writeRam 0x41 (status `clearBit` 2) bus.io
  | 0xFF50 == addr = do
      -- 0xFF50 disables boot ROM
      b <- readByte0xFF50 bus
      writeRam 0x50 (val .|. b) bus.io
  | 0xFF00 < addr && addr < 0xFF80 = do
      writeRam (addr - 0xFF00) val bus.io
  | 0xFF80 <= addr && addr < 0xFFFF = do
      writeRam (addr - 0xFF80) val bus.hram
  | addr == 0xFFFF = do
      writeIORef bus.ie val
  | otherwise = return ()

readPPUMode :: Ram -> IO Word8
readPPUMode io = do
  status <- readRam 0x41 io
  return $ status .&. 0x03

readByteHighMemory :: Word8 -> Bus -> IO Word8
readByteHighMemory offset = readByte (0xFF00 + fromIntegral offset)

writeByteHighMemory :: Word8 -> Word8 -> Bus -> IO ()
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
  writeByte (getHL regs) val bus
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

-- for PPU
readVRam :: Address -> Bus -> IO Word8
readVRam addr bus =
  readRam (addr - 0x8000) bus.vram

-- https://gbdev.io/pandocs/LCDC.html
readLcdC :: Int -> Bus -> IO Bool
readLcdC index bus = do
  b <- readByte 0xFF40 bus
  return $ (b .>>. index .&. 0x01) == 1

isLcdOn :: Bus -> IO Bool
isLcdOn = readLcdC 7

readLcdCWindowTileMapArea :: Bus -> IO Address
readLcdCWindowTileMapArea bus = do
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

isLcdCObjEnable :: Bus -> IO Bool
isLcdCObjEnable = readLcdC 1

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
  return $ (b .>>. index .&. 0x01) == 1

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

readSCXInt :: Bus -> IO Int
readSCXInt bus = fromIntegral <$> readSCX bus

readBGPalette :: Bus -> IO ColorPalette
readBGPalette = readByte 0xFF47

readOBP0Palette :: Bus -> IO ColorPalette
readOBP0Palette = readByte 0xFF48

readOBP1Palette :: Bus -> IO ColorPalette
readOBP1Palette = readByte 0xFF49

readObjPalette :: Bool -> Bus -> IO ColorPalette
readObjPalette False = readOBP0Palette
readObjPalette True = readOBP1Palette

readWY :: Bus -> IO Word8
readWY = readByte 0xFF4A

readWX :: Bus -> IO Word8
readWX = readByte 0xFF4B

readWXInt :: Bus -> IO Int
readWXInt bus = fromIntegral <$> readWX bus

-- in order make PPU state readable by CPU from Bus
syncPPU :: Word8 -> Word8 -> Bus -> IO ()
syncPPU ly mode bus = updateSTATInterrupt bus $ syncPPURegisters ly mode bus

-- LCDC writes update LY/mode as part of the same STAT edge comparison.
syncPPURegisters :: Word8 -> Word8 -> Bus -> IO ()
syncPPURegisters ly mode bus = do
  -- TODO: there are other things need update
  writeRam 0x44 ly bus.io
  lyc <- readRam 0x45 bus.io

  status <- readRam 0x41 bus.io
  let status' = if ly == lyc then status `setBit` 2 else status `clearBit` 2
  writeRam 0x41 (status' .&. 0xFC .|. mode) bus.io

readSTATInterruptLine :: Bus -> IO Bool
readSTATInterruptLine bus = do
  lcdOn <- isLcdOn bus
  status <- readRam 0x41 bus.io
  let mode = status .&. 0x03
  return $ lcdOn &&
        ((status `testBit` 3 && mode == 0)
          || (status `testBit` 4 && mode == 1)
          || (status `testBit` 5 && mode == 2)
          || (status `testBit` 6 && status `testBit` 2))

-- Compare the signal before and after a complete register update.
updateSTATInterrupt :: Bus -> IO () -> IO ()
updateSTATInterrupt bus update = do
  oldLine <- readSTATInterruptLine bus
  update
  newLine <- readSTATInterruptLine bus
  when (not oldLine && newLine) $ writeIF LCDStat True bus

-- TODO: OAM DMA transfer
-- https://gbdev.io/pandocs/OAM_DMA_Transfer.html

-- OAM read for PPU
data OAMObjectPosition = OAMObjectPosition
  { yPos :: Int
  , xPos :: Int
  }

data OAMObjectAttributes = OAMObjectAttributes
  { tileIndex :: TileIndex
  , attributes :: Word8
  }

readOAMPosition :: Word8 -> Bus -> IO OAMObjectPosition
readOAMPosition i bus = do
  let addr = fromIntegral i * 4
  y <- readRam addr bus.oam
  x <- readRam (addr + 1) bus.oam
  return $ OAMObjectPosition (fromIntegral y) (fromIntegral x)

-- OAM has 16 bits bus, it can read 2 bytes at a time
readOAMAttributes :: Address -> Bus -> IO OAMObjectAttributes
readOAMAttributes addr bus = do
  tileIndex <- readRam addr bus.oam
  attributes <- readRam (addr + 1) bus.oam
  return $ OAMObjectAttributes tileIndex attributes

type TileIndex = Word8

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

-- Interruption
data Interrupt = VBlank | LCDStat | TimerInt | Serial | JoypadInt  deriving (Show, Eq, Enum)

interruptAddress :: Interrupt -> Address
interruptAddress VBlank = 0x40
interruptAddress LCDStat = 0x48
interruptAddress TimerInt = 0x50
interruptAddress Serial = 0x58
interruptAddress JoypadInt = 0x60

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
                                ) [VBlank .. JoypadInt]

readIE :: Bus -> IO Interrupts
readIE = filterInterrupts $ readByte 0xFFFF

writeIE :: Interrupt -> Bool -> Bus -> IO ()
writeIE int val bus = do
  b <- readByte 0xFFFF bus
  let bit = fromEnum int
  let update = if val then setBit else clearBit
  writeByte 0xFFFF (b `update` bit) bus
  return ()

readIF :: Bus -> IO Interrupts
readIF = filterInterrupts $ readByte 0xFF0F

writeIF :: Interrupt -> Bool -> Bus -> IO ()
writeIF int val bus = do
  b <- readByte 0xFF0F bus
  let bit = fromEnum int
  let update = if val then setBit else clearBit
  writeByte 0xFF0F (b `update` bit) bus
  return ()

readJOYP :: Bus -> IO Word8
readJOYP = readByte 0xFF00

data JoypadKey = AKey | BKey | SelectKey | StartKey | RightKey | LeftKey | UpKey | DownKey deriving (Show, Eq, Enum)

requestJoypadInt :: Word8 -> Word8 -> Bus -> IO ()
requestJoypadInt oldVal val bus =
  when ((oldVal .&. complement val .&. 0x0F) /= 0) $ writeIF JoypadInt True bus

setJoypad :: JoypadKey -> Bool -> Bus -> IO ()
setJoypad key b bus = do
  oldJoyp <- readJOYP bus
  oldVal <- readIORef bus.joypad
  writeIORef bus.joypad $ if b then clearBit oldVal (fromEnum key)
                          else setBit oldVal (fromEnum key)
  joyp <- readJOYP bus
  requestJoypadInt oldJoyp joyp bus

resetDIV :: Bus -> IO ()
resetDIV = detectFallingEdge (\bus -> writeIORef bus.systemCounter 0)

increaseTIMA :: Bus -> IO ()
increaseTIMA bus = do
  tima <- readRam 5 bus.io
  let tima' = tima + 1
  writeRam 5 tima' bus.io
  when (tima' == 0) $ writeIORef bus.timaOverflow TIMAOverflowDelay

triggerTimaOverflow :: Bus -> IO ()
triggerTimaOverflow bus = do
    timaOverflow <- readIORef bus.timaOverflow
    case timaOverflow of
      NoTIMAOverflow -> return ()
      TIMAOverflowDelay -> do
        tma <- readTMA bus
        writeRam 5 tma bus.io
        writeIORef bus.timaOverflow TIMAOverflowReloading
        writeIF TimerInt True bus
      TIMAOverflowReloading ->
        clearTIMAOverflow bus

readTMA :: Bus -> IO Word8
readTMA bus = readRam 6 bus.io

readTAC :: Bus -> IO Word8
readTAC bus = readRam 7 bus.io

increaseTimer :: Word8 -> Bus -> IO ()
increaseTimer 0 _ = return ()
increaseTimer n bus = do
  triggerTimaOverflow bus

  detectFallingEdge (\bus' -> do
    counter <- readIORef bus'.systemCounter
    writeIORef bus'.systemCounter $ counter + 1) bus

  increaseTimer (n - 1) bus

clearTIMAOverflow :: Bus -> IO ()
clearTIMAOverflow bus = writeIORef bus.timaOverflow NoTIMAOverflow

timerSignal :: Word8 -> Word16 -> Bool
timerSignal tac counter =
  let selectedBit = case tac .&. 0x03 of
        0 -> 7
        1 -> 1
        2 -> 3
        _ -> 5
  in testBit tac 2 && testBit counter selectedBit

detectFallingEdge :: (Bus -> IO a) -> Bus -> IO a
detectFallingEdge f bus = do
    oldCounter <- readIORef bus.systemCounter
    oldTac <- readTAC bus
    res <- f bus
    counter <- readIORef bus.systemCounter
    tac <- readTAC bus
    let oldSignal = timerSignal oldTac oldCounter
        newSignal = timerSignal tac counter
    when (oldSignal && not newSignal) $ increaseTIMA bus
    return res
