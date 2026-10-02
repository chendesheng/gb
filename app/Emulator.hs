{-# LANGUAGE BangPatterns #-}

module Emulator (Emulator (..), powerOn, advanceFrame) where

import qualified CPU
import qualified Data.ByteString.Lazy as BL
import qualified PPU

data Emulator = Emulator
  { cpu :: !CPU.CPU
  , ppu :: !PPU.PPU
  , cycleDebt :: !Int
  }

powerOn :: BL.ByteString -> BL.ByteString -> IO Emulator
powerOn boot cartridge = do
  cpu <- CPU.initCPU boot cartridge
  pure $ Emulator cpu (PPU.initPPU cpu.bus) 0

-- Keep CPU and PPU in sync using the same scheduler as the boot test.
-- Carry the final instruction's overshoot into the next frame.
advanceFrame :: Emulator -> IO Emulator
advanceFrame emulator = go (70224 - emulator.cycleDebt) emulator.cpu emulator.ppu
  where
    go remaining !cpu !ppu
      | remaining <= 0 = pure $ Emulator cpu ppu (-remaining)
      | otherwise = do
          (cpu', cycles) <- CPU.execute cpu
          ppu' <- PPU.execute cycles ppu
          go (remaining - fromIntegral cycles) cpu' ppu'
