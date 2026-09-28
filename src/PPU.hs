module PPU (execute, FIFOPixel(..), PixelFIFO(..), PPU(..), initPPU) where

import Prelude hiding (replicate)
import Bus
import Color
import Data.Vector (Vector, replicate, toList)
import Data.Word

type Pixel = Word8

-- One dot = one PPU clock. One scanline = 456 dots. One frame = 154 lines = 70224 dots.

-- 160*144
newtype Display = Display (Vector (Vector Pixel))

initDisplay :: Display
initDisplay = Display (replicate 144 (replicate 160 0))

data FIFOPixel = FIFOPixel
  { color :: ColorIndex, -- 0 - 3
    palette :: Int, -- 0 - 7
    spritePriority :: Int,
    backgroundPriority :: Int
  }

data PixelFIFO = PixelFIFO {oam :: [FIFOPixel], background :: [FIFOPixel]}

instance Show Display where
  show (Display pixels) =
    unlines $ toList $ fmap showRow pixels
    where
      showRow :: Vector Pixel -> String
      showRow = toList . fmap showPixel

      showPixel :: Pixel -> Char
      showPixel 0 = 'A'
      showPixel 1 = 'B'
      showPixel 2 = 'C'
      showPixel _ = 'D'

-- color :: Pixel -> Word32
-- color 0 = 0x9BBC0F
-- color 1 = 0x8BAC0F
-- color 2 = 0x306230
-- color _ = 0x0F380F

data PPU = PPU
  { dots :: Word64,
    lcdOn :: Bool,
    display :: Display,
    bus :: Bus
  }

initPPU :: Bus -> PPU
initPPU = PPU 0 False initDisplay

-- newtype Tile = Tile (Vector Word8) -- length 16

advance :: Word64 -> PPU -> PPU
advance duration ppu = ppu{dots = ppu.dots + duration}

step :: PPU -> IO PPU
step ppu =
  -- TOOD
  return $ advance 1 ppu

execute :: Word64 -> PPU -> IO PPU
execute 0 ppu = return ppu
execute duration ppu = do
  let wasOn = ppu.lcdOn
  isOn <- isLcdOn ppu.bus
  case (wasOn, isOn) of
    (False, True) -> execute duration ppu{dots=0, lcdOn=True}
    (True,  False) -> return ppu{lcdOn=False}
    (False, False) -> return ppu
    (True, True) -> do
      ppu1 <- step ppu
      execute (duration - 1) ppu1

 
