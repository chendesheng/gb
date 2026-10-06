module Emulator (
  Emulator (..),
  powerOn,
  advanceFrame,
  setJoypadKey,
  ) where

import qualified CPU
import Data.ByteString.Lazy (ByteString)
import qualified PPU
import Bus (initBus, JoypadKey, setJoypad)
import CPU (initCPU, CPU)
import PPU (initPPU, PPU)

data Emulator = Emulator
  { cpu :: !CPU
  , ppu :: !PPU
  , cycleDebt :: !Int
  }

powerOn :: ByteString -> ByteString -> IO Emulator
powerOn boot cartridge = do
  bus <- initBus boot cartridge
  let cpu = initCPU bus
  let ppu = initPPU cpu.bus
  pure $ Emulator cpu ppu 0

-- Keep CPU and PPU in sync using the same scheduler as the boot test.
-- Carry the final instruction's overshoot into the next frame.
advanceFrame :: Emulator -> IO Emulator
advanceFrame emulator = go (70224 - emulator.cycleDebt) emulator
  where
    go remaining em
      | remaining <= 0 = pure $ em{cycleDebt = -remaining}
      | otherwise = do
          (cpu', cycles) <- CPU.execute em.cpu
          ppu' <- PPU.execute cycles em.ppu
          go (remaining - fromIntegral cycles) em{cpu=cpu', ppu=ppu'}

setJoypadKey :: JoypadKey -> Bool -> Emulator -> IO ()
setJoypadKey key press emulator = do
  setJoypad key press emulator.cpu.bus
